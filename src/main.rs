#![feature(async_closure)]
use tokio::sync::oneshot;
use futures::future::FutureExt;
use futures::{join, select, pin_mut};

mod server;
mod params;
mod page;
mod heartbeat;

use heartbeat::heartbeat_loop;
use server::server;

const HOST: [u8; 4] = [127, 0, 0, 1];
const PORT: u16 = 8000;

#[tokio::main]
async fn main() {
    let socket_addr = (HOST, PORT).into();

    // Provide a way for the server to signal that it's quit
    let (quit, wait_quit) = oneshot::channel::<()>();

    // Wait for the server to quit, then emit a `()`
    let wait_quit = async {
        wait_quit.await.unwrap_or(())
    }.fuse();

    // Run the server and the heartbeat loop concurrently
    let main_loop = async {
        join!(server(socket_addr, quit),
              heartbeat_loop())
    }.fuse();

    pin_mut!(main_loop, wait_quit);
    select! {
        ((), ()) = main_loop => (),
        () = wait_quit => (),
    }
}
