use reqwest;
use std::time::{Instant, Duration};
use tokio::time;

#[tokio::main]
async fn main() {
    let client = reqwest::Client::new();
    let mut interval = time::interval(Duration::from_millis(16));
    let mut ok = true;
    while ok {
        let start = Instant::now();
        for angle in 0_usize .. 360 {
            interval.tick().await;
            let body = format!(
                r#"<div id="container" style="overflow: hidden; margin: 0px;
               padding: 0px; height: 100vh; background: hsl({angle}, 30%, 50%);
               width: 100vw; text-align: center; position: relative;">
               <span style="position: absolute; top: 50%; transform:
               translate(-50%, -50%) rotate({angle}deg); font-family:
               Helvetica Neue; font-weight: 200; font-size: 250pt; color: white;
               background: rgba(0, 0, 0, 0.4); border-radius: 300pt;
               border: none; padding: 100pt; width: 550pt; text-shadow:
               0 0 25pt black">{angle}°</span></div>"#,
                angle = angle);
            let result =
                client.post("http://localhost:1123").body(body).send().await;
            if let Err(e) = result {
                eprintln!("{}", e);
                ok = false;
                break;
            }
        }
        let elapsed = start.elapsed();
        println!("{:.1} fps", 360.0 / elapsed.as_secs_f32());
    }
}
