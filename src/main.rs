use futures::future::FutureExt;
use futures::{select, pin_mut};

mod server;
mod params;
mod page;
mod heartbeat;

use heartbeat::heartbeat_loop;
use server::server;

const HOST: [u8; 4] = [127, 0, 0, 1];
const INTERFACE_PORT: u16 = 2628; // B-O-A-T on a telephone keypad
const CONTROL_PORT:   u16 = 7245; // S-A-I-L on a telephone keypad

// TODO: specify options on the command line

#[tokio::main]
async fn main() {
    let control_server =
        server(server::Mode::Control, (HOST, CONTROL_PORT).into()).fuse();
    let interface_server =
        server(server::Mode::Interface, (HOST, INTERFACE_PORT).into()).fuse();
    let heartbeat = heartbeat_loop().fuse();

    pin_mut!(control_server, interface_server, heartbeat);
    select! {
        () = control_server => (),
        () = interface_server => (),
        () = heartbeat => (),
    }
}
