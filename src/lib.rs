#![allow(dead_code)]

#[macro_use]
extern crate lazy_static;

use std::fmt::Write;
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

pub fn format_in_threes(f: &mut std::fmt::Formatter, n: u64)
                        -> std::fmt::Result {
    if n >= 1000 {
        format_in_threes(f, n / 1000)?;
        write!(f, "_{:03}", n % 1000)
    } else {
        write!(f, "{}", n)
    }
}

pub fn format_in_threes_signed(f: &mut std::fmt::Formatter, n: i64)
                               -> std::fmt::Result {
    if n < 0 {
        write!(f, "-")?;
        format_in_threes(f, -n as u64)
    } else {
        format_in_threes(f, n as u64)
    }
}

pub fn octets(data: &[u8]) -> String {
    let mut result = String::new();
    for i in data.iter() {
        write!(result, "{:02x}", *i).unwrap();
    }
    result
}

pub fn hex(n: u64) -> String {
    let mut result = String::new();
    write!(result, "0x{:x}", n).unwrap();
    result
}

pub fn errsym(err: &std::io::Error) -> String {
    if let Some(errno) = err.raw_os_error() {
        match errno {
            libc::EPERM => "EPERM".to_string(),
            libc::ENOENT => "ENOENT".to_string(),
            libc::ESRCH => "ESRCH".to_string(),
            libc::EINTR => "EINTR".to_string(),
            libc::EIO => "EIO".to_string(),
            libc::ENXIO => "ENXIO".to_string(),
            libc::E2BIG => "E2BIG".to_string(),
            libc::ENOEXEC => "ENOEXEC".to_string(),
            libc::EBADF => "EBADF".to_string(),
            libc::ECHILD => "ECHILD".to_string(),
            libc::EAGAIN => "EAGAIN".to_string(),
            libc::ENOMEM => "ENOMEM".to_string(),
            libc::EACCES => "EACCES".to_string(),
            libc::EFAULT => "EFAULT".to_string(),
            libc::ENOTBLK => "ENOTBLK".to_string(),
            libc::EBUSY => "EBUSY".to_string(),
            libc::EEXIST => "EEXIST".to_string(),
            libc::EXDEV => "EXDEV".to_string(),
            libc::ENODEV => "ENODEV".to_string(),
            libc::ENOTDIR => "ENOTDIR".to_string(),
            libc::EISDIR => "EISDIR".to_string(),
            libc::EINVAL => "EINVAL".to_string(),
            libc::ENFILE => "ENFILE".to_string(),
            libc::EMFILE => "EMFILE".to_string(),
            libc::ENOTTY => "ENOTTY".to_string(),
            libc::ETXTBSY => "ETXTBSY".to_string(),
            libc::EFBIG => "EFBIG".to_string(),
            libc::ENOSPC => "ENOSPC".to_string(),
            libc::ESPIPE => "ESPIPE".to_string(),
            libc::EROFS => "EROFS".to_string(),
            libc::EMLINK => "EMLINK".to_string(),
            libc::EPIPE => "EPIPE".to_string(),
            libc::EDOM => "EDOM".to_string(),
            libc::ERANGE => "ERANGE".to_string(),
            libc::EDEADLK => "EDEADLK".to_string(),
            libc::ENAMETOOLONG => "ENAMETOOLONG".to_string(),
            libc::ENOLCK => "ENOLCK".to_string(),
            libc::ENOSYS => "ENOSYS".to_string(),
            libc::ENOTEMPTY => "ENOTEMPTY".to_string(),
            libc::ELOOP => "ELOOP".to_string(),
            libc::ENOMSG => "ENOMSG".to_string(),
            libc::EIDRM => "EIDRM".to_string(),
            libc::ECHRNG => "ECHRNG".to_string(),
            libc::EL2NSYNC => "EL2NSYNC".to_string(),
            libc::EL3HLT => "EL3HLT".to_string(),
            libc::EL3RST => "EL3RST".to_string(),
            libc::ELNRNG => "ELNRNG".to_string(),
            libc::EUNATCH => "EUNATCH".to_string(),
            libc::ENOCSI => "ENOCSI".to_string(),
            libc::EL2HLT => "EL2HLT".to_string(),
            libc::EBADE => "EBADE".to_string(),
            libc::EBADR => "EBADR".to_string(),
            libc::EXFULL => "EXFULL".to_string(),
            libc::ENOANO => "ENOANO".to_string(),
            libc::EBADRQC => "EBADRQC".to_string(),
            libc::EBADSLT => "EBADSLT".to_string(),
            libc::EBFONT => "EBFONT".to_string(),
            libc::ENOSTR => "ENOSTR".to_string(),
            libc::ENODATA => "ENODATA".to_string(),
            libc::ETIME => "ETIME".to_string(),
            libc::ENOSR => "ENOSR".to_string(),
            libc::ENONET => "ENONET".to_string(),
            libc::ENOPKG => "ENOPKG".to_string(),
            libc::EREMOTE => "EREMOTE".to_string(),
            libc::ENOLINK => "ENOLINK".to_string(),
            libc::EADV => "EADV".to_string(),
            libc::ESRMNT => "ESRMNT".to_string(),
            libc::ECOMM => "ECOMM".to_string(),
            libc::EPROTO => "EPROTO".to_string(),
            libc::EMULTIHOP => "EMULTIHOP".to_string(),
            libc::EDOTDOT => "EDOTDOT".to_string(),
            libc::EBADMSG => "EBADMSG".to_string(),
            libc::EOVERFLOW => "EOVERFLOW".to_string(),
            libc::ENOTUNIQ => "ENOTUNIQ".to_string(),
            libc::EBADFD => "EBADFD".to_string(),
            libc::EREMCHG => "EREMCHG".to_string(),
            libc::ELIBACC => "ELIBACC".to_string(),
            libc::ELIBBAD => "ELIBBAD".to_string(),
            libc::ELIBSCN => "ELIBSCN".to_string(),
            libc::ELIBMAX => "ELIBMAX".to_string(),
            libc::ELIBEXEC => "ELIBEXEC".to_string(),
            libc::EILSEQ => "EILSEQ".to_string(),
            libc::ERESTART => "ERESTART".to_string(),
            libc::ESTRPIPE => "ESTRPIPE".to_string(),
            libc::EUSERS => "EUSERS".to_string(),
            libc::ENOTSOCK => "ENOTSOCK".to_string(),
            libc::EDESTADDRREQ => "EDESTADDRREQ".to_string(),
            libc::EMSGSIZE => "EMSGSIZE".to_string(),
            libc::EPROTOTYPE => "EPROTOTYPE".to_string(),
            libc::ENOPROTOOPT => "ENOPROTOOPT".to_string(),
            libc::EPROTONOSUPPORT => "EPROTONOSUPPORT".to_string(),
            libc::ESOCKTNOSUPPORT => "ESOCKTNOSUPPORT".to_string(),
            libc::EOPNOTSUPP => "EOPNOTSUPP".to_string(),
            libc::EPFNOSUPPORT => "EPFNOSUPPORT".to_string(),
            libc::EAFNOSUPPORT => "EAFNOSUPPORT".to_string(),
            libc::EADDRINUSE => "EADDRINUSE".to_string(),
            libc::EADDRNOTAVAIL => "EADDRNOTAVAIL".to_string(),
            libc::ENETDOWN => "ENETDOWN".to_string(),
            libc::ENETUNREACH => "ENETUNREACH".to_string(),
            libc::ENETRESET => "ENETRESET".to_string(),
            libc::ECONNABORTED => "ECONNABORTED".to_string(),
            libc::ECONNRESET => "ECONNRESET".to_string(),
            libc::ENOBUFS => "ENOBUFS".to_string(),
            libc::EISCONN => "EISCONN".to_string(),
            libc::ENOTCONN => "ENOTCONN".to_string(),
            libc::ESHUTDOWN => "ESHUTDOWN".to_string(),
            libc::ETOOMANYREFS => "ETOOMANYREFS".to_string(),
            libc::ETIMEDOUT => "ETIMEDOUT".to_string(),
            libc::ECONNREFUSED => "ECONNREFUSED".to_string(),
            libc::EHOSTDOWN => "EHOSTDOWN".to_string(),
            libc::EHOSTUNREACH => "EHOSTUNREACH".to_string(),
            libc::EALREADY => "EALREADY".to_string(),
            libc::EINPROGRESS => "EINPROGRESS".to_string(),
            libc::ESTALE => "ESTALE".to_string(),
            libc::EUCLEAN => "EUCLEAN".to_string(),
            libc::ENOTNAM => "ENOTNAM".to_string(),
            libc::ENAVAIL => "ENAVAIL".to_string(),
            libc::EISNAM => "EISNAM".to_string(),
            libc::EREMOTEIO => "EREMOTEIO".to_string(),
            libc::EDQUOT => "EDQUOT".to_string(),
            libc::ENOMEDIUM => "ENOMEDIUM".to_string(),
            libc::EMEDIUMTYPE => "EMEDIUMTYPE".to_string(),
            libc::ECANCELED => "ECANCELED".to_string(),
            libc::ENOKEY => "ENOKEY".to_string(),
            libc::EKEYEXPIRED => "EKEYEXPIRED".to_string(),
            libc::EKEYREVOKED => "EKEYREVOKED".to_string(),
            libc::EKEYREJECTED => "EKEYREJECTED".to_string(),
            libc::EOWNERDEAD => "EOWNERDEAD".to_string(),
            libc::ENOTRECOVERABLE => "ENOTRECOVERABLE".to_string(),
            libc::ERFKILL => "ERFKILL".to_string(),
            libc::EHWPOISON => "EHWPOISON".to_string(),
            0 => "0".to_string(),
            _ => err.to_string(),
        }
    } else {
        err.to_string()
    }
}

#[derive(Debug, Copy, Clone)]
pub struct UID(u64);

impl UID {
    pub fn new() -> UID {
        let mut events_body = TRACE_DB.mut_body();
        let uid = UID(events_body.seqno);
        events_body.seqno += 1;     // TODO: forking support
        uid
    }
}

impl std::fmt::Display for UID {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        format_in_threes(f, self.0)
    }
} // impl Display for UID

impl PartialEq for UID {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
} // impl PartialEq for UID

impl Eq for UID {}

impl PartialOrd for UID {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }

    fn lt(&self, other: &Self) -> bool {
        self.0.lt(&other.0)
    }

    fn le(&self, other: &Self) -> bool {
        self.0.le(&other.0)
    }

    fn gt(&self, other: &Self) -> bool {
        self.0.gt(&other.0)
    }

    fn ge(&self, other: &Self) -> bool {
        self.0.ge(&other.0)
    }
}

impl Ord for UID {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }

    fn max(self, other: Self) -> Self {
        UID(self.0.max(other.0))
    }

    fn min(self, other: Self) -> Self {
        UID(self.0.min(other.0))
    }

    fn clamp(self, min: Self, max: Self) -> Self {
        UID(self.0.clamp(min.0, max.0))
    }
} // impl Ord for UID

pub struct Event(bool);

impl Event {
    pub fn new(selected: bool) -> Event {
        Event(selected)
    }

    pub fn is_selected(&self) -> bool {
        self.0
    }

    pub fn reselect(&mut self, selected: bool) {
        self.0 = selected;
    }
} // impl Event

pub type EventRef = Arc<RwLock<Event>>;

pub struct EventsBody {
    events: Vec<(&'static str, EventRef)>,
    selector: Box<dyn Selector + Sync + Send>,
    emitter: Box<dyn Emitter + Sync + Send>,
    seqno: u64,
}

impl EventsBody {
    pub fn emit(&self, trace_line: String) {
        self.emitter.emit(trace_line);
    }
} // impl EventsBody

pub struct Events(RwLock<EventsBody>);

impl Events {
    pub fn mut_body(&self) -> RwLockWriteGuard<EventsBody> {
        self.0.write().unwrap()
    }

    pub fn body(&self) -> RwLockReadGuard<EventsBody> {
        self.0.read().unwrap()
    }
}

pub trait Selector {
    fn select(&self, event: String) -> bool;
}

pub struct NullSelector {}

impl NullSelector {
    pub fn new() -> NullSelector {
        NullSelector {}
    }
} // impl NullSelector

impl Selector for NullSelector {
    fn select(&self, _event: String) -> bool {
        false
    }
} // impl Selector for NullSelector

pub struct RegexSelector {
    include: Option<regex::Regex>,
    exclude: Option<regex::Regex>,
}

impl RegexSelector {
    pub fn new(include: Option<&str>, exclude: Option<&str>) -> RegexSelector {
        RegexSelector {
            include: include.map(|re| regex::Regex::new(re).unwrap()),
            exclude: exclude.map(|re| regex::Regex::new(re).unwrap()),
        }
    }
} // impl RegexSelector

impl Selector for RegexSelector {
    fn select(&self, event: String) -> bool {
        self.include.iter().any(|ptn| ptn.is_match(event.as_str())) &&
            !self.exclude.iter().any(|ptn| ptn.is_match(event.as_str()))
    }
} // impl Selector for RegexSelector

pub trait Emitter {
    fn emit(&self, trace_line: String);
}

pub struct StderrEmitter {}

impl StderrEmitter {
    pub fn new() -> StderrEmitter {
        StderrEmitter {}
    }
} // impl StderrEmitter

impl Emitter for StderrEmitter {
    fn emit(&self, trace_line: String) {
        eprintln!("{}", trace_line);
    }
} // impl Emitter for StderrEmitter

pub struct NullEmitter {}

impl NullEmitter {
    pub fn new() -> NullEmitter {
        NullEmitter {}
    }
} // impl NullEmitter

impl Emitter for NullEmitter {
    fn emit(&self, _trace_line: String) {
    }
} // impl Emitter for NullEmitter

lazy_static! {
    pub static ref TRACE_DB: Events = Events(RwLock::new(EventsBody {
        events: Vec::new(),
        selector: Box::new(NullSelector::new()),
        emitter: Box::new(StderrEmitter::new()),
        seqno: 0,
    }));
}

pub fn set_selector(selector: Box<dyn Selector + Sync + Send>) {
    let mut events_body = TRACE_DB.mut_body();
    for (key, value) in events_body.events.iter() {
        (*value).write().unwrap().reselect(selector.select(key.to_string()));
    }
    events_body.selector = selector;
}

pub fn set_emitter(emitter: Box<dyn Emitter + Sync + Send>) {
    TRACE_DB.mut_body().emitter = emitter;
}

pub fn declare_event(name: &'static str) -> EventRef {
    let mut events_body = TRACE_DB.mut_body();
    let link = Arc::new(RwLock::new(
        Event::new(events_body.selector.select(name.to_string()))));
    events_body.events.push((name, link.clone()));
    link
}

pub fn timestamp() -> String {
    chrono::offset::Utc::now().format(
        "%Y-%m-%d %H:%M:%S.%6f").to_string()
}

pub fn urlencode(repr: String) -> String {
    urlencoding::encode(repr.as_str()).to_string()
}

#[macro_export]
macro_rules! r3_declare_event {
    ($event:ident) => {
        lazy_static! {
            static ref EVENT: $crate::EventRef =
                $crate::declare_event(stringify!($event));
        }
    }
}

#[macro_export]
macro_rules! r3_dashify_id {
    ($id:ident) => {
        stringify!($id).replace("_", "-")
    };
}

#[macro_export]
macro_rules! r3_event_is_selected {
    ($event:ident) => {
        EVENT.read().unwrap().is_selected()
    };
}

#[macro_export]
macro_rules! r3_format_event_arg {
    ($arg:ident, $val:expr) => {
        {
            let enc = $crate::urlencode(($val).to_string());
            format!("{}={}", $crate::r3_dashify_id!($arg), enc).to_string()
        }
    };
}

#[macro_export]
macro_rules! r3_format_event_args {
    ($event:ident { $($arg:ident: $val:expr,)* }) => {
        vec![
            $crate::timestamp(),
            $crate::r3_dashify_id!($event),
            $($crate::r3_format_event_arg!($arg, $val),)*
        ].join(" ")
    };
}

#[macro_export]
macro_rules! emit {
    ($line:expr) => {
        // leave TRACE_DB locked for strict interleaving
        $crate::TRACE_DB.body().emit($line);
    }
}

#[macro_export]
macro_rules! TRACE {
    ($event:ident { $($arg:ident: $val:expr),* }) => {
        {
            $crate::r3_declare_event!($event);
            if $crate::r3_event_is_selected!($event) {
                $crate::emit!($crate::r3_format_event_args!(
                    $event { $($arg: $val,)* }));
            }
        }
    };
    ($event:ident { $($arg:ident: $val:expr,)* }) => {
        {
            $crate::r3_declare_event!($event);
            if $crate::r3_event_is_selected!($event) {
                $crate::emit!($crate::r3_format_event_args!(
                    $event { $($arg: $val,)* }));
            }
        }
    };
    ($event:ident) => {
        {
            $crate::r3_declare_event!($event);
            if $crate::r3_event_is_selected!($event) {
                $crate::emit!($crate::r3_format_event_args!(
                    $event {}));
            }
        }
    };
}

#[macro_export]
macro_rules! TRACE_ENABLED {
    ($event:ident) => {
        {
            $crate::r3_declare_event!($event);
            $crate::r3_event_is_selected!($event)
        }
    };
}
