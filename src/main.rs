#![windows_subsystem = "windows"]
use futures::future::FutureExt;
use futures::{select, pin_mut};
use std::thread;
use tokio::runtime::Runtime;

mod server;
mod params;
mod page;
mod heartbeat;
#[cfg(feature = "windowing")]
mod windowing;

use heartbeat::heartbeat_loop;
use server::server;
use windowing::WindowManager;

const HOST: [u8; 4] = [127, 0, 0, 1];
const INTERFACE_PORT: u16 = 8000;
const CONTROL_PORT:   u16 = 8001;

// TODO: specify options on the command line

async fn async_main(window_manager: WindowManager) {
    let control_server =
        server(server::Mode::Control, (HOST, CONTROL_PORT).into(), window_manager.clone()).fuse();
    let interface_server =
        server(server::Mode::Interface, (HOST, INTERFACE_PORT).into(), window_manager).fuse();
    let heartbeat = heartbeat_loop().fuse();

    pin_mut!(control_server, interface_server, heartbeat);
    select! {
        () = control_server => (),
        () = interface_server => (),
        () = heartbeat => (),
    }
}

fn main () {
    let (window_manager, mut event_loop) = windowing::manager();
    thread::spawn(move || {
        Runtime::new().expect("Tokio runtime error!")
            .block_on(async_main(window_manager));
    });
    event_loop.run();
}
