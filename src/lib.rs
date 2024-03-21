//! A helper for building DBus services

#![allow(clippy::new_without_default)]

use std::collections::{HashMap, VecDeque};
use std::rc::Rc;

use rustbus::connection::Timeout;
use rustbus::message_builder::{
    HeaderFlags, MarshalledMessage, MarshalledMessageBody, MessageBodyParser,
};
use rustbus::params::{Param, Variant};
use rustbus::wire::unmarshal::traits::Variant as UnVariant;
use rustbus::{DuplexConn, MessageType, Signature};

pub use rustbus;
pub use rustbus_service_macros::{Args, ReturnArgs};

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
                MessageType::Error => {
                    let error_name = msg.dynheader.error_name.expect("error without error_name");
                    let error_message = msg.body.parser().get::<&str>().ok();
                    eprintln!(
                        "dbus error: {error_name}: {}",
                        error_message.unwrap_or("<no message>")
                    );
                }
                MessageType::Call => {
                    if let Some(cb) = get_call_handler(&self.root, &msg) {
                        cb(MethodContext {
                            service: self,
                            conn,
                            state,
                            msg: &msg,
                            object_path: msg.dynheader.object.as_deref().unwrap(),
                        })?;
                    } else if msg.flags & HeaderFlags::NoReplyExpected.into_raw() == 0 {
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

pub trait ReturnArgs: Sized {
    type Ty<'a>;
    fn push(
        val: Self::Ty<'_>,
        msg: &mut MarshalledMessageBody,
    ) -> Result<(), rustbus::wire::errors::MarshalError>;
    fn introspect(buf: &mut Vec<MethodArgument>);
}

impl Args for () {
    type Ty<'a> = ();
    fn parse(_: MessageBodyParser) -> Result<Self::Ty<'_>, rustbus::wire::errors::UnmarshalError> {
        Ok(())
    }
    fn introspect(_buf: &mut Vec<MethodArgument>) {}
}

impl ReturnArgs for () {
    type Ty<'a> = ();
    fn push(
        _val: Self::Ty<'_>,
        _msg: &mut MarshalledMessageBody,
    ) -> Result<(), rustbus::wire::errors::MarshalError> {
        Ok(())
    }
    fn introspect(_buf: &mut Vec<MethodArgument>) {}
}

type Error = rustbus::connection::Error;
type MethodCallCb<D> = Rc<dyn Fn(MethodContext<D>) -> Result<(), Error>>;
type PropGetCb<D> = Box<dyn Fn(PropContext<D>) -> Param<'static, 'static>>;
type PropSetCb<D> = Box<dyn Fn(PropContext<D>, UnVariant)>;
type ArgsIntrospect = fn(&mut Vec<MethodArgument>);

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

        #[derive(ReturnArgs)]
        struct PropsChangedSignal<'a> {
            interface_name: &'a str,
            changed_properties: HashMap<&'a str, Variant<'a, 'a>>,
            invalidated_properties: &'a [String],
        }

        let props_iface = InterfaceImp::new("org.freedesktop.DBus.Properties")
            .with_method::<GetPropArgs, GetPropReturnArgs>("Get", get_prop_cb)
            .with_method::<GetAllPropsArgs, GetAllPropsReturnArgs>("GetAll", get_all_props_cb)
            .with_method::<SetPropArgs, ()>("Set", set_prop_cb)
            .with_signal::<PropsChangedSignal>("PropertiesChanged");
        object.add_interface(props_iface);

        let introspectable_iface =
            InterfaceImp::new("org.freedesktop.DBus.Introspectable")
                .with_method::<(), IntrospectReturnArgs>("Introspect", introspect_cb);
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

    /// Remove a child from this object.
    ///
    /// Returns the removed object, if it exists.
    pub fn remove_child(&mut self, name: &str) -> Option<Self> {
        self.children.remove(name)
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

/// A DBus interface implementation.
pub struct InterfaceImp<D> {
    name: Box<str>,
    methods: HashMap<Box<str>, MethodImp<D>>,
    props: HashMap<Box<str>, PropertyImp<D>>,
    signals: Vec<SignalImp>,
}

impl<D> InterfaceImp<D> {
    /// Create a new empty interface.
    pub fn new(interface: impl Into<Box<str>>) -> Self {
        Self {
            name: interface.into(),
            methods: HashMap::new(),
            props: HashMap::new(),
            signals: Vec::new(),
        }
    }

    /// Add a method.
    pub fn with_method<A, Ra>(
        mut self,
        name: impl Into<Box<str>>,
        handler: impl for<'a, 'b> Fn(&'b mut MethodContext<'a, D>, A::Ty<'a>) -> Ra::Ty<'b> + 'static,
    ) -> Self
    where
        A: Args,
        Ra: ReturnArgs,
    {
        self.methods.insert(
            name.into(),
            MethodImp {
                handler: Rc::new(move |mut ctx| {
                    let send_reply = ctx.msg.flags & HeaderFlags::NoReplyExpected.into_raw() == 0;
                    let Ok(args) = A::parse(ctx.msg.body.parser()) else {
                        if send_reply {
                            let resp =
                                rustbus::standard_messages::invalid_args(&ctx.msg.dynheader, None);
                            ctx.conn.send.send_message_write_all(&resp)?;
                        }
                        return Ok(());
                    };
                    let mut resp = ctx.msg.dynheader.make_response();
                    let ret_args = handler(&mut ctx, args);
                    if send_reply {
                        Ra::push(ret_args, &mut resp.body)?;
                        ctx.conn.send.send_message_write_all(&resp)?;
                    }
                    Ok(())
                }),
                introspect: |x| {
                    A::introspect(x);
                    Ra::introspect(x);
                },
            },
        );
        self
    }

    /// Add a property.
    pub fn with_prop<T, R, W>(mut self, name: impl Into<Box<str>>, access: Access<R, W>) -> Self
    where
        T: Signature + Into<Param<'static, 'static>>,
        R: Fn(PropContext<D>) -> T + 'static,
        W: Fn(PropContext<D>, UnVariant) + 'static,
    {
        self.props.insert(
            name.into(),
            PropertyImp {
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

    /// Add a signal. Used for introspection only.
    pub fn with_signal<Ra: ReturnArgs>(mut self, name: impl Into<Box<str>>) -> Self {
        let name = name.into();
        self.signals.push(SignalImp {
            name,
            introspect: |x| Ra::introspect(x),
        });
        self
    }
}

struct MethodImp<D> {
    handler: MethodCallCb<D>,
    introspect: ArgsIntrospect,
}

struct SignalImp {
    name: Box<str>,
    introspect: ArgsIntrospect,
}

struct PropertyImp<D> {
    signature: rustbus::signature::Type,
    access: Access<PropGetCb<D>, PropSetCb<D>>,
}

/// A method argument for introspection.
pub struct MethodArgument {
    pub name: &'static str,
    pub is_out: bool,
    pub signature: rustbus::signature::Type,
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

#[derive(ReturnArgs)]
struct GetPropReturnArgs<'a> {
    value: Variant<'a, 'a>,
}

fn get_prop_cb<'a, D: 'static>(
    ctx: &'a mut MethodContext<D>,
    args: GetPropArgs,
) -> GetPropReturnArgs<'a> {
    let object = ctx.service.get_object(ctx.object_path).unwrap();
    let iface = object.interfaces.get(args.iface_name).unwrap();
    let prop = iface.props.get(args.prop_name).unwrap();

    let pctx = PropContext {
        conn: ctx.conn,
        state: ctx.state,
        object_path: ctx.object_path,
        name: args.prop_name,
    };

    match &prop.access {
        Access::Write(_) => todo!(),
        Access::Read(get) | Access::ReadWrite(get, _) => {
            let val = get(pctx);
            GetPropReturnArgs {
                value: Variant {
                    sig: val.sig(),
                    value: val,
                },
            }
        }
    }
}

#[derive(Args)]
struct GetAllPropsArgs<'a> {
    iface_name: &'a str,
}

#[derive(ReturnArgs)]
struct GetAllPropsReturnArgs<'a> {
    props: HashMap<&'a str, Variant<'a, 'a>>,
}

fn get_all_props_cb<'a, D: 'static>(
    ctx: &'a mut MethodContext<D>,
    args: GetAllPropsArgs,
) -> GetAllPropsReturnArgs<'a> {
    let object = ctx.service.get_object(ctx.object_path).unwrap();
    let iface = object.interfaces.get(args.iface_name).unwrap();

    let mut props = HashMap::<&str, Variant>::new();

    for (prop_name, prop) in &iface.props {
        let ctx = PropContext {
            conn: ctx.conn,
            state: ctx.state,
            object_path: ctx.object_path,
            name: prop_name,
        };

        match &prop.access {
            Access::Write(_) => (),
            Access::Read(get) | Access::ReadWrite(get, _) => {
                let val = get(ctx);
                props.insert(
                    prop_name,
                    Variant {
                        sig: val.sig(),
                        value: val,
                    },
                );
            }
        }
    }

    GetAllPropsReturnArgs { props }
}

#[derive(Args)]
struct SetPropArgs<'a> {
    iface_name: &'a str,
    prop_name: &'a str,
    value: UnVariant<'a, 'a>,
}

fn set_prop_cb<D: 'static>(ctx: &mut MethodContext<D>, args: SetPropArgs) {
    let object = ctx.service.get_object(ctx.object_path).unwrap();
    let iface = object.interfaces.get(args.iface_name).unwrap();

    let prop = iface.props.get(args.prop_name).unwrap();

    let pctx = PropContext {
        conn: ctx.conn,
        state: ctx.state,
        object_path: ctx.object_path,
        name: args.prop_name,
    };

    match &prop.access {
        Access::Read(_) => todo!(),
        Access::Write(set) | Access::ReadWrite(_, set) => set(pctx, args.value),
    }
}

#[derive(ReturnArgs)]
struct IntrospectReturnArgs {
    xml_data: String,
}

fn introspect_cb<D: 'static>(ctx: &mut MethodContext<D>, _args: ()) -> IntrospectReturnArgs {
    let object = ctx.service.get_object(ctx.object_path).unwrap();

    let mut xml = String::new();
    let mut args_buf = Vec::new();
    xml.push_str(r#"<!DOCTYPE node PUBLIC "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN" "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd">"#);
    xml.push_str("<node>");
    for iface in object.interfaces.values() {
        xml.push_str(r#"<interface name=""#);
        xml.push_str(&iface.name);
        xml.push_str(r#"">"#);
        for (method_name, method) in &iface.methods {
            args_buf.clear();
            (method.introspect)(&mut args_buf);
            xml.push_str(r#"<method name=""#);
            xml.push_str(method_name);
            xml.push_str(r#"">"#);
            for arg in &args_buf {
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
        for signal in &iface.signals {
            args_buf.clear();
            (signal.introspect)(&mut args_buf);
            xml.push_str(r#"<signal name=""#);
            xml.push_str(&signal.name);
            xml.push_str(r#"">"#);
            for arg in &args_buf {
                xml.push_str(r#"<arg name=""#);
                xml.push_str(arg.name);
                xml.push_str(r#"" type=""#);
                arg.signature.to_str(&mut xml);
                xml.push_str(r#""/>"#);
            }
            xml.push_str(r#"</signal>"#);
        }
        for (prop_name, prop) in &iface.props {
            xml.push_str(r#"<property name=""#);
            xml.push_str(prop_name);
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

    IntrospectReturnArgs { xml_data: xml }
}
