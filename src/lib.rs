//! A helper for building DBus services

#![allow(clippy::new_without_default)]

use std::collections::{HashMap, VecDeque};
use std::rc::Rc;

use rustbus::connection::Timeout;
use rustbus::message_builder::{MarshalledMessage, MessageBodyParser};
use rustbus::params::{Param, Variant};
use rustbus::wire::unmarshal::traits::Variant as UnVariant;
use rustbus::{DuplexConn, MessageType, Signature};

pub use rustbus;
pub use rustbus_service_macros::Args;

/// A helper for building DBus services.
///
/// Dispatches method calls to the provided callbacks and automatically provides the implementation
/// of `org.freedesktop.DBus.Introspectable` and `org.freedesktop.DBus.Properties` interfaces.
pub struct Service<D> {
    root: Object<D>,
    queue: VecDeque<MarshalledMessage>,
}

pub struct MethodContext<'a, D> {
    pub service: &'a mut Service<D>,
    pub conn: &'a mut DuplexConn,
    pub state: &'a mut D,
    pub msg: &'a MarshalledMessage,
    pub object_path: &'a str,
}

pub struct PropContext<'a, D> {
    pub conn: &'a mut DuplexConn,
    pub state: &'a mut D,
    pub object_path: &'a str,
    pub name: &'a str,
}

impl<D: 'static> Service<D> {
    /// Create a new service helper.
    pub fn new() -> Self {
        Self {
            root: Object::new(),
            queue: VecDeque::new(),
        }
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

    /// Get a reply message with a given serial and queue all the other messages.
    pub fn get_reply(
        &mut self,
        conn: &mut DuplexConn,
        serial: u32,
        timeout: Timeout,
    ) -> Result<MarshalledMessage, rustbus::connection::Error> {
        assert!(matches!(timeout, Timeout::Infinite), "unimplemented");
        if let Some(i) = self
            .queue
            .iter()
            .position(|x| x.dynheader.response_serial == Some(serial))
        {
            Ok(self.queue.remove(i).unwrap())
        } else {
            loop {
                let msg = conn.recv.get_next_message(Timeout::Infinite)?;
                if msg.dynheader.response_serial == Some(serial) {
                    break Ok(msg);
                }
                self.queue.push_back(msg);
            }
        }
    }

    /// Receive messages and dispatch method calls.
    pub fn run(
        &mut self,
        conn: &mut DuplexConn,
        state: &mut D,
        timeout: Timeout,
    ) -> Result<(), rustbus::connection::Error> {
        assert!(matches!(timeout, Timeout::Nonblock), "unimplemented");
        loop {
            let msg = match self.queue.pop_front() {
                Some(msg) => msg,
                None => match conn.recv.get_next_message(Timeout::Nonblock) {
                    Ok(msg) => msg,
                    Err(rustbus::connection::Error::TimedOut) => return Ok(()),
                    Err(e) => return Err(e),
                },
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
                            conn,
                            state,
                            msg: &msg,
                            object_path: msg.dynheader.object.as_deref().unwrap(),
                        });
                    } else {
                        let resp = rustbus::standard_messages::unknown_method(&msg.dynheader);
                        conn.send.send_message_write_all(&resp)?;
                    }
                }
                MessageType::Reply => todo!(),
                MessageType::Invalid => todo!(),
            }
        }
    }
}

pub trait Args: Sized {
    type Ty<'a>;
    fn parse(
        parser: MessageBodyParser,
    ) -> Result<Self::Ty<'_>, rustbus::wire::errors::UnmarshalError>;
    fn introspect(buf: &mut Vec<MethodArgument>);
}

impl Args for () {
    type Ty<'a> = ();

    fn parse(_: MessageBodyParser) -> Result<Self::Ty<'_>, rustbus::wire::errors::UnmarshalError> {
        Ok(())
    }

    fn introspect(_buf: &mut Vec<MethodArgument>) {}
}

type MethodCallCb<D> = Rc<dyn Fn(MethodContext<D>)>;
type PropGetCb<D> = Box<dyn Fn(PropContext<D>) -> Param<'static, 'static>>;
type PropSetCb<D> = Box<dyn Fn(PropContext<D>, UnVariant)>;

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

        let get_method =
            MethodImp::new::<GetPropArgs, _>("Get", get_prop_cb).add_return_arg::<Variant>("value");
        let get_all_method = MethodImp::new::<GetAllPropsArgs, _>("GetAll", get_all_props_cb)
            .add_return_arg::<HashMap<String, Variant>>("props");
        let set_method = MethodImp::new::<SetPropArgs, _>("Set", set_prop_cb);
        let props_changed_signal = SignalImp::new("PropertiesChanged")
            .add_arg::<String>("interface_name")
            .add_arg::<HashMap<String, Variant>>("changed_properties")
            .add_arg::<&[String]>("invalidated_properties");
        let props_iface = InterfaceImp::new("org.freedesktop.DBus.Properties")
            .add_method(get_method)
            .add_method(get_all_method)
            .add_method(set_method)
            .add_signal(props_changed_signal);
        object.add_interface(props_iface);

        let introspect_method = MethodImp::new::<(), _>("Introspect", introspect_cb)
            .add_return_arg::<String>("xml_data");
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
            None if rel_path.is_empty() => Some(self),
            None => self.children.get(rel_path),
            Some((name, rest)) => self.children.get(name).and_then(|obj| obj.get_child(rest)),
        }
    }

    fn get_child_mut<'a>(&'a mut self, rel_path: &'_ str) -> Option<&'a mut Self> {
        match rel_path.split_once('/') {
            None if rel_path.is_empty() => Some(self),
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
    signals: HashMap<Box<str>, SignalImp>,
    props: HashMap<Box<str>, PropertyImp<D>>,
}

impl<D> InterfaceImp<D> {
    pub fn new(interface: impl Into<Box<str>>) -> Self {
        Self {
            name: interface.into(),
            methods: HashMap::new(),
            signals: HashMap::new(),
            props: HashMap::new(),
        }
    }

    pub fn add_method(mut self, method: MethodImp<D>) -> Self {
        self.methods.insert(method.name.clone(), method);
        self
    }

    pub fn add_signal(mut self, signal: SignalImp) -> Self {
        self.signals.insert(signal.name.clone(), signal);
        self
    }

    pub fn add_prop<T, R, W>(mut self, name: impl Into<Box<str>>, access: Access<R, W>) -> Self
    where
        T: Signature + Into<Param<'static, 'static>>,
        R: Fn(PropContext<D>) -> T + 'static,
        W: Fn(PropContext<D>, UnVariant) + 'static,
    {
        let name = name.into();
        self.props.insert(
            name.clone(),
            PropertyImp {
                name,
                signature: T::signature(),
                access: match access {
                    Access::Read(r) => Access::Read(Box::new(move |ctx| r(ctx).into())),
                    Access::Write(w) => Access::Write(Box::new(w)),
                    Access::ReadWrite(r, w) => {
                        Access::ReadWrite(Box::new(move |ctx| r(ctx).into()), Box::new(w))
                    }
                },
            },
        );
        self
    }
}

pub struct MethodImp<D> {
    name: Box<str>,
    handler: MethodCallCb<D>,
    #[allow(clippy::type_complexity)]
    introspect: Box<dyn Fn(&mut Vec<MethodArgument>)>,
    return_args: Vec<MethodArgument>,
}

impl<D> MethodImp<D> {
    pub fn new<A, F>(name: impl Into<Box<str>>, handler: F) -> Self
    where
        A: Args,
        F: for<'a, 'b> Fn(MethodContext<'a, D>, A::Ty<'b>) + 'static,
    {
        let name = name.into();
        MethodImp {
            name,
            handler: Rc::new(move |ctx| {
                let a = A::parse(ctx.msg.body.parser()).unwrap();
                handler(ctx, a);
            }),
            introspect: Box::new(|x| A::introspect(x)),
            return_args: Vec::new(),
        }
    }

    pub fn add_return_arg<T: Signature>(mut self, name: &'static str) -> Self {
        self.return_args.push(MethodArgument {
            name,
            is_out: true,
            signature: T::signature(),
        });
        self
    }
}

pub struct MethodArgument {
    pub name: &'static str,
    pub is_out: bool,
    pub signature: rustbus::signature::Type,
}

pub struct SignalImp {
    name: Box<str>,
    args: Vec<SignalArgument>,
}

impl SignalImp {
    pub fn new(name: impl Into<Box<str>>) -> Self {
        let name = name.into();
        SignalImp {
            name,
            args: Vec::new(),
        }
    }

    pub fn add_arg<T: Signature>(mut self, name: impl Into<Box<str>>) -> Self {
        self.args.push(SignalArgument {
            name: name.into(),
            signature: T::signature(),
        });
        self
    }
}

struct SignalArgument {
    name: Box<str>,
    signature: rustbus::signature::Type,
}

pub struct PropertyImp<D> {
    name: Box<str>,
    signature: rustbus::signature::Type,
    access: Access<PropGetCb<D>, PropSetCb<D>>,
}

pub enum Access<R, W> {
    Read(R),
    Write(W),
    ReadWrite(R, W),
}

#[derive(Args)]
struct GetPropArgs<'a> {
    iface_name: &'a str,
    prop_name: &'a str,
}

fn get_prop_cb<D: 'static>(ctx: MethodContext<D>, args: GetPropArgs) {
    let object = ctx.service.get_object(ctx.object_path).unwrap();
    let iface = object.interfaces.get(args.iface_name).unwrap();
    let prop = iface.props.get(args.prop_name).unwrap();

    let pctx = PropContext {
        conn: ctx.conn,
        state: ctx.state,
        object_path: ctx.object_path,
        name: &prop.name,
    };

    match &prop.access {
        Access::Write(_) => todo!(),
        Access::Read(get) | Access::ReadWrite(get, _) => {
            let val = get(pctx);
            let val = Variant {
                sig: val.sig(),
                value: val,
            };
            let mut resp = ctx.msg.dynheader.make_response();
            resp.body.push_param(val).unwrap();
            ctx.conn.send.send_message_write_all(&resp).unwrap();
        }
    }
}

#[derive(Args)]
struct GetAllPropsArgs<'a> {
    iface_name: &'a str,
}

fn get_all_props_cb<D: 'static>(ctx: MethodContext<D>, args: GetAllPropsArgs) {
    let object = ctx.service.get_object(ctx.object_path).unwrap();
    let iface = object.interfaces.get(args.iface_name).unwrap();

    let mut props = HashMap::<&str, Variant>::new();

    for prop in iface.props.values() {
        let ctx = PropContext {
            conn: ctx.conn,
            state: ctx.state,
            object_path: ctx.object_path,
            name: &prop.name,
        };

        match &prop.access {
            Access::Write(_) => (),
            Access::Read(get) | Access::ReadWrite(get, _) => {
                let val = get(ctx);
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
    ctx.conn.send.send_message_write_all(&resp).unwrap();
}

#[derive(Args)]
struct SetPropArgs<'a> {
    iface_name: &'a str,
    prop_name: &'a str,
    value: UnVariant<'a, 'a>,
}

fn set_prop_cb<D: 'static>(ctx: MethodContext<D>, args: SetPropArgs) {
    let object = ctx.service.get_object(ctx.object_path).unwrap();
    let iface = object.interfaces.get(args.iface_name).unwrap();

    let prop = iface.props.get(args.prop_name).unwrap();

    let pctx = PropContext {
        conn: ctx.conn,
        state: ctx.state,
        object_path: ctx.object_path,
        name: &prop.name,
    };

    match &prop.access {
        Access::Read(_) => todo!(),
        Access::Write(set) | Access::ReadWrite(_, set) => set(pctx, args.value),
    }

    ctx.conn
        .send
        .send_message_write_all(&ctx.msg.dynheader.make_response())
        .unwrap();
}

fn introspect_cb<D: 'static>(ctx: MethodContext<D>, _args: ()) {
    let object = ctx.service.get_object(ctx.object_path).unwrap();

    let mut xml = String::new();
    let mut args_buf = Vec::new();
    xml.push_str(r#"<!DOCTYPE node PUBLIC "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN" "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd">"#);
    xml.push_str("<node>");
    for iface in object.interfaces.values() {
        xml.push_str(r#"<interface name=""#);
        xml.push_str(&iface.name);
        xml.push_str(r#"">"#);
        for method in iface.methods.values() {
            args_buf.clear();
            (method.introspect)(&mut args_buf);
            xml.push_str(r#"<method name=""#);
            xml.push_str(&method.name);
            xml.push_str(r#"">"#);
            for arg in args_buf.iter().chain(&method.return_args) {
                xml.push_str(r#"<arg name=""#);
                xml.push_str(arg.name);
                xml.push_str(r#"" type=""#);
                arg.signature.to_str(&mut xml);
                xml.push_str(r#"" direction=""#);
                xml.push_str(if arg.is_out { "out" } else { "in" });
                xml.push_str(r#""/>"#);
            }
            xml.push_str(r#"</method>"#);
        }
        for method in iface.signals.values() {
            xml.push_str(r#"<signal name=""#);
            xml.push_str(&method.name);
            xml.push_str(r#"">"#);
            for arg in &method.args {
                xml.push_str(r#"<arg name=""#);
                xml.push_str(&arg.name);
                xml.push_str(r#"" type=""#);
                arg.signature.to_str(&mut xml);
                xml.push_str(r#""/>"#);
            }
            xml.push_str(r#"</signal>"#);
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
    ctx.conn.send.send_message_write_all(&resp).unwrap();
}
