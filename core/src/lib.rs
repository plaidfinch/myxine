//! This package is the heart of the [Myxine GUI
//! server](https://github.com/GaloisInc/myxine). It does not contain any code
//! for directly implementing the web server or browser-side JavaScript.
//!
//! You probably only need to depend on this library if you are yourself
//! creating an alternative to the Myxine server: to build clients to the Myxine
//! server, see the documentation for its various client libraries.

mod page;
mod session;

pub use session::{Session, Config};
pub use page::{Page, content::Command, Response, RefreshMode};
pub use page::subscription::{Subscription, Event};
