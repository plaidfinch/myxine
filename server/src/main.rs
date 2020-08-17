use structopt::StructOpt;

mod params;
mod server;

#[derive(Debug, StructOpt)]
#[structopt(
    about =
        "A local web server to help you make interactive applications\n\
         in your browser using any language under the sea!\n\
         \n\
         DOCUMENTATION:\
         \n    https://github.com/kwf/myxine")]
struct Options {
    /// Run on this port
    #[structopt(short, long, default_value = "1123")]
    port: u16,
}

#[tokio::main]
async fn main() {
    let options = Options::from_args();
    if let Err(err) = server::run(([0, 0, 0, 0], options.port)).await {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}
