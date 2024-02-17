//! A helper for building DBus services

use std::collections::HashMap;
use std::os::fd::{AsRawFd, RawFd};
use std::rc::Rc;

use rustbus::connection::Timeout;
use rustbus::message_builder::MarshalledMessage;
use rustbus::params::{Param, Variant};
use rustbus::wire::unmarshal::traits::Variant as UnVariant;
use rustbus::{DuplexConn, MessageType, Signature};

pub use rustbus;

/// A helper for building DBus services.
///
/// Dispatches method calls to the provided callbacks and automatically provides the implementation
/// of `org.freedesktop.DBus.Introspectable` and `org.freedesktop.DBus.Properties` interfaces.
pub struct Service<D> {
    conn: DuplexConn,
    root: Object<D>,
}

pub struct MethodContext<'a, D> {
    pub service: &'a mut Service<D>,
    pub state: &'a mut D,
    pub msg: &'a MarshalledMessage,
    pub object_path: &'a str,
}

impl<D> AsRawFd for Service<D> {
    fn as_raw_fd(&self) -> RawFd {
        self.conn.as_raw_fd()
    }
}

impl<D: 'static> Service<D> {
    /// Create a new service helper.
    pub fn new(conn: DuplexConn) -> Self {
        Self {
            conn,
            root: Object::new(),
        }
    }

    /// Get the underlying connection.
    pub fn conn_mut(&mut self) -> &mut DuplexConn {
        &mut self.conn
    }

    /// Get the root object.
    pub fn root(&self) -> &Object<D> {
        &self.root
    }

    /// Get the root object.
    pub fn root_mut(&mut self) -> &mut Object<D> {
        &mut self.root
    }

    /// Get a object at the given path.
    pub fn get_object(&self, path: &str) -> Option<&Object<D>> {
        self.root.get_child(path.strip_prefix('/')?)
    }

    /// Get a object at the given path.
    pub fn get_object_mut(&mut self, path: &str) -> Option<&mut Object<D>> {
        self.root.get_child_mut(path.strip_prefix('/')?)
    }

    /// Receive messages and dispatch method calls.
    pub fn run(
        &mut self,
        state: &mut D,
        timeout: Timeout,
    ) -> Result<(), rustbus::connection::Error> {
        assert!(matches!(timeout, Timeout::Nonblock), "unimplemented");
        loop {
            let msg = match self.conn.recv.get_next_message(Timeout::Nonblock) {
                Ok(msg) => msg,
                Err(rustbus::connection::Error::TimedOut) => return Ok(()),
                Err(e) => return Err(e),
            };

            match msg.typ {
                MessageType::Signal => {
                    eprintln!("todo: handle signal: {:?}", msg.dynheader.member);
                }
                MessageType::Error => todo!(),
                MessageType::Call => {
                    if let Some(cb) = get_call_handler(&self.root, &msg) {
                        cb(MethodContext {
                            service: self,
                            state,
                            msg: &msg,
                            object_path: msg.dynheader.object.as_deref().unwrap(),
                        });
                    } else {
                        let mut resp = rustbus::standard_messages::unknown_method(&msg.dynheader);
                        self.conn.send.send_message_write_all(&mut resp)?;
                    }
                }
                MessageType::Reply => todo!(),
                MessageType::Invalid => todo!(),
            }
        }
    }
}

type MethodCallCb<D> = Rc<dyn Fn(MethodContext<D>)>;

fn get_call_handler<D: 'static>(
    root: &Object<D>,
    msg: &MarshalledMessage,
) -> Option<MethodCallCb<D>> {
    let path = msg.dynheader.object.as_deref()?.strip_prefix('/')?;
    let object = root.get_child(path)?;
    let iface = object.interfaces.get(msg.dynheader.interface.as_deref()?)?;
    let method = iface.methods.get(msg.dynheader.member.as_deref()?)?;
    Some(method.handler.clone())
}

/// A service-side representation of a DBus object.
pub struct Object<D> {
    interfaces: HashMap<Box<str>, InterfaceImp<D>>,
    children: HashMap<Box<str>, Self>,
}

impl<D: 'static> Object<D> {
    /// Create a new object which implements only `org.freedesktop.DBus.Introspectable` and
    /// `org.freedesktop.DBus.Properties`.
    pub fn new() -> Self {
        let mut object = Self {
            interfaces: HashMap::new(),
            children: HashMap::new(),
        };

        // TODO: add Get and PropertiesChanged
        let get_all_method = MethodImp::new("GetAll", get_all_props_cb)
            .add_arg::<String>("interface_name", false)
            .add_arg::<HashMap<String, Variant>>("props", true);
        let set_method = MethodImp::new("Set", set_prop_cb)
            .add_arg::<String>("interface_name", false)
            .add_arg::<String>("property_name", false)
            .add_arg::<Variant>("value", false);
        let props_iface = InterfaceImp::new("org.freedesktop.DBus.Properties")
            .add_method(get_all_method)
            .add_method(set_method);
        object.add_interface(props_iface);

        let introspect_method =
            MethodImp::new("Introspect", introspect_cb).add_arg::<String>("xml_data", true);
        let introspectable_iface =
            InterfaceImp::new("org.freedesktop.DBus.Introspectable").add_method(introspect_method);
        object.add_interface(introspectable_iface);

        object
    }

    /// Add a new interface implementation to this object.
    pub fn add_interface(&mut self, interface: InterfaceImp<D>) {
        self.interfaces.insert(interface.name.clone(), interface);
    }

    /// Add a child to this object.
    pub fn add_child(&mut self, name: impl Into<Box<str>>, object: Self) {
        self.children.insert(name.into(), object);
    }

    fn get_child<'a>(&'a self, rel_path: &'_ str) -> Option<&'a Self> {
        match rel_path.split_once('/') {
            None if rel_path == "" => Some(self),
            None => self.children.get(rel_path),
            Some((name, rest)) => self.children.get(name).and_then(|obj| obj.get_child(rest)),
        }
    }

    fn get_child_mut<'a>(&'a mut self, rel_path: &'_ str) -> Option<&'a mut Self> {
        match rel_path.split_once('/') {
            None if rel_path == "" => Some(self),
            None => self.children.get_mut(rel_path),
            Some((name, rest)) => self
                .children
                .get_mut(name)
                .and_then(|obj| obj.get_child_mut(rest)),
        }
    }
}

pub struct InterfaceImp<D> {
    name: Box<str>,
    methods: HashMap<Box<str>, MethodImp<D>>,
    props: HashMap<Box<str>, PropertyImp<D>>,
}

impl<D> InterfaceImp<D> {
    pub fn new(interface: impl Into<Box<str>>) -> Self {
        Self {
            name: interface.into(),
            methods: HashMap::new(),
            props: HashMap::new(),
        }
    }

    pub fn add_method(mut self, method: MethodImp<D>) -> Self {
        self.methods.insert(method.name.clone(), method);
        self
    }

    pub fn add_prop<T, R, W>(mut self, name: impl Into<Box<str>>, access: Access<R, W>) -> Self
    where
        T: Signature + Into<Param<'static, 'static>>,
        R: Fn(&str, &mut D) -> T + 'static,
        W: Fn(&str, &mut D, UnVariant) + 'static,
    {
        let name = name.into();
        self.props.insert(
            name.clone(),
            PropertyImp {
                name,
                signature: T::signature(),
                access: match access {
                    Access::Read(_) => todo!(),
                    Access::Write(_) => todo!(),
                    Access::ReadWrite(r, w) => Access::ReadWrite(
                        Box::new(move |name, state| r(name, state).into()),
                        Box::new(move |name, state, param| w(name, state, param)),
                    ),
                },
            },
        );
        self
    }
}

pub struct MethodImp<D> {
    name: Box<str>,
    handler: MethodCallCb<D>,
    args: Vec<MethodArgument>,
}

impl<D> MethodImp<D> {
    pub fn new<F>(name: impl Into<Box<str>>, handler: F) -> Self
    where
        F: Fn(MethodContext<D>) + 'static,
    {
        let name = name.into();
        MethodImp {
            name,
            handler: Rc::new(handler),
            args: Vec::new(),
        }
    }

    pub fn add_arg<T: Signature>(mut self, name: impl Into<Box<str>>, is_out: bool) -> Self {
        self.args.push(MethodArgument {
            name: name.into(),
            is_out,
            signature: T::signature(),
        });
        self
    }
}

struct MethodArgument {
    name: Box<str>,
    is_out: bool,
    signature: rustbus::signature::Type,
}

pub struct PropertyImp<D> {
    name: Box<str>,
    signature: rustbus::signature::Type,
    access: Access<
        Box<dyn Fn(&str, &mut D) -> Param<'static, 'static>>,
        Box<dyn Fn(&str, &mut D, UnVariant)>,
    >,
}

pub enum Access<R, W> {
    Read(R),
    Write(W),
    ReadWrite(R, W),
}

fn get_all_props_cb<D: 'static>(ctx: MethodContext<D>) {
    let object = ctx.service.get_object(ctx.object_path).unwrap();
    let iface_name = ctx.msg.body.parser().get::<&str>().unwrap();
    let iface = object.interfaces.get(iface_name).unwrap();

    let mut props = HashMap::<&str, Variant>::new();

    for prop in iface.props.values() {
        match &prop.access {
            Access::Read(_) => todo!(),
            Access::Write(_) => todo!(),
            Access::ReadWrite(get, _) => {
                let val = get(ctx.object_path, ctx.state);
                props.insert(
                    &prop.name,
                    Variant {
                        sig: val.sig(),
                        value: val,
                    },
                );
            }
        }
    }

    let mut resp = ctx.msg.dynheader.make_response();
    resp.body.push_param(props).unwrap();
    ctx.service
        .conn
        .send
        .send_message_write_all(&mut resp)
        .unwrap();
}

fn set_prop_cb<D: 'static>(ctx: MethodContext<D>) {
    let mut parser = ctx.msg.body.parser();
    let iface = parser.get::<&str>().unwrap();
    let prop = parser.get::<&str>().unwrap();
    let value = parser.get::<UnVariant>().unwrap();

    let object = ctx.service.get_object(ctx.object_path).unwrap();
    let iface = object.interfaces.get(iface).unwrap();

    let prop = iface.props.get(prop).unwrap();
    match &prop.access {
        Access::Read(_) => todo!(),
        Access::Write(_) => todo!(),
        Access::ReadWrite(_, set) => set(ctx.object_path, ctx.state, value),
    }

    ctx.service
        .conn
        .send
        .send_message_write_all(&mut ctx.msg.dynheader.make_response())
        .unwrap();
}

fn introspect_cb<D: 'static>(ctx: MethodContext<D>) {
    let object = ctx.service.get_object(ctx.object_path).unwrap();

    let mut xml = String::new();
    xml.push_str(r#"<!DOCTYPE node PUBLIC "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN" "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd">"#);
    xml.push_str("<node>");
    for iface in object.interfaces.values() {
        xml.push_str(r#"<interface name=""#);
        xml.push_str(&iface.name);
        xml.push_str(r#"">"#);
        for method in iface.methods.values() {
            xml.push_str(r#"<method name=""#);
            xml.push_str(&method.name);
            xml.push_str(r#"">"#);
            for arg in &method.args {
                xml.push_str(r#"<arg name=""#);
                xml.push_str(&arg.name);
                xml.push_str(r#"" type=""#);
                arg.signature.to_str(&mut xml);
                xml.push_str(r#"" direction=""#);
                xml.push_str(if arg.is_out { "out" } else { "in" });
                xml.push_str(r#""/>"#);
            }
            xml.push_str(r#"</method>"#);
        }
        for prop in iface.props.values() {
            xml.push_str(r#"<property name=""#);
            xml.push_str(&prop.name);
            xml.push_str(r#"" type=""#);
            prop.signature.to_str(&mut xml);
            xml.push_str(r#"" access=""#);
            xml.push_str(match prop.access {
                Access::Read(_) => "read",
                Access::Write(_) => "write",
                Access::ReadWrite(_, _) => "readwrite",
            });
            xml.push_str(r#""/>"#);
        }
        xml.push_str(r#"</interface>"#);
    }
    for child in object.children.keys() {
        xml.push_str(r#"<node name=""#);
        xml.push_str(child);
        xml.push_str(r#""/>"#);
    }
    xml.push_str("</node>");

    // TODO: signals
    // <signal name="PropertiesChanged">
    //   <arg name="interface_name" type="s"/>
    //   <arg name="changed_properties" type="a{sv}"/>
    //   <arg name="invalidated_properties" type="as"/>
    // </signal>

    let mut resp = ctx.msg.dynheader.make_response();
    resp.body.push_param(&xml).unwrap();
    ctx.service
        .conn
        .send
        .send_message_write_all(&mut resp)
        .unwrap();
}
