//! This package is the heart of the [Myxine GUI
//! server](https://github.com/GaloisInc/myxine). It does not contain any code
//! for directly implementing the web server or browser-side JavaScript.
//!
//! You probably only need to depend on this library if you are yourself
//! creating an alternative to the Myxine server: to build clients to the Myxine
//! server, see the documentation for its various client libraries.

mod page;
mod session;

pub use page::subscription::{Event, Subscription};
pub use page::{content::Command, Page, RefreshMode, Response};
pub use session::{Config, Session};
