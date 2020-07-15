use structopt::StructOpt;

mod server;

#[derive(Debug, StructOpt)]
struct Options {
    /// Run on this port
    #[structopt(short, long, default_value = "1123")]
    port: u16,
}

#[tokio::main]
async fn main() {
    let options = Options::from_args();
    if let Err(err) = server::run(([127, 0, 0, 1], options.port)).await {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}
