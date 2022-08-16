#![allow(dead_code)]

#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;
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
    events: HashMap<&'static str, EventRef>,
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
        events: HashMap::new(),
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
    events_body.events.insert(name, link.clone());
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
            static ref $event: $crate::EventRef = {
                $crate::declare_event(stringify!($event))
            };
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
        $event.read().unwrap().is_selected()
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
        $crate::r3_declare_event!($event);
        if $crate::r3_event_is_selected!($event) {
            $crate::emit!($crate::r3_format_event_args!(
                $event { $($arg: $val,)* }));
        }
    };
    ($event:ident { $($arg:ident: $val:expr,)* }) => {
        $crate::r3_declare_event!($event);
        if $crate::r3_event_is_selected!($event) {
            $crate::emit!($crate::r3_format_event_args!(
                $event { $($arg: $val,)* }));
        }
    };
    ($event:ident) => {
        $crate::r3_declare_event!($event);
        if $crate::r3_event_is_selected!($event) {
            $crate::emit!($crate::r3_format_event_args!(
                $event {}));
        }
    };
}
