use futures::future::FutureExt;
use futures::{select, pin_mut};
use structopt::StructOpt;

mod server;
mod params;
mod page;
mod heartbeat;
mod events;

use heartbeat::heartbeat_loop;
use server::server;

const HOST: [u8; 4] = [127, 0, 0, 1];

#[derive(Debug, StructOpt)]
struct Options {
    /// Run on this port
    #[structopt(short, long, default_value = "1123")]
    port: u16,
}

#[tokio::main]
async fn main() {
    let options = Options::from_args();

    let server = server((HOST, options.port).into()).fuse();
    let heartbeat = heartbeat_loop().fuse();

    pin_mut!(server, heartbeat);
    select! {
        () = server => (),
        () = heartbeat => (),
    }
}
