use std::collections::HashMap;
use reqwest;
use serde_json::{json, Value};
use futures::{Stream, StreamExt};
use bytes::{Bytes, Buf};
use std::marker::Unpin;

/// A stream of `Bytes` chunks paired with a buffer, to allow us to read from it
/// in a line-oriented way rather than in chunks.
#[derive(Debug)]
pub struct BufByteStream<E, S: Unpin + Stream<Item = Result<Bytes, E>>> {
    stream: S,
    buffer: Bytes,
}

impl<E, S: Unpin + Stream<Item = Result<Bytes, E>>> BufByteStream<E, S> {
    /// Wrap a stream of chunks or errors (as, for example, would be returned
    /// from `reqwest::Response::bytes_stream`) in a buffered async reader.
    pub fn from_result_stream(stream: S) -> BufByteStream<E, S> {
        BufByteStream{stream, buffer: Bytes::new()}
    }

    /// Return a `Vec` of bytes representing all bytes in this reader up to *and
    /// including* the specified "stop" byte.
    async fn read_until(&mut self, stop: u8) -> Vec<u8> {
        let mut result = Vec::new();
        loop {
            let mut index: usize = 0;
            for byte in self.buffer.iter() {
                index += 1;
                result.push(*byte);
                if *byte == stop {
                    self.buffer.advance(index);
                    return result;
                }
            }
            if let Some(Ok(chunk)) = self.stream.next().await {
                self.buffer = chunk;
            } else {
                return result;
            }
        }
    }

    /// Read a line of the wrapped stream, up to *and including* the newline
    /// character that terminated the line (or none if it reached the end of the
    /// stream without a newline).
    async fn read_line(&mut self) -> Vec<u8> {
        self.read_until(b'\n').await
    }
}

/// A UI event, as sent from the server, contains an optional `id`, `event`
/// type, and some `data` which in our case always comes in the form of a map
/// from string keys to unknown JSON `Value`s.
#[derive(Debug, Clone)]
pub struct Event {
    pub id: Option<String>,
    pub event: Option<String>,
    pub data: HashMap<String, Value>,
}

impl Event {
    /// Make a new, empty event.
    pub fn new() -> Event {
        Event {
            id: None,
            event: None,
            data: HashMap::new(),
        }
    }
}

/// A stream of `Event`s read from the network.
#[derive(Debug)]
pub struct EventStream<S: Unpin + Stream<Item = Result<Bytes, reqwest::Error>>>(
    BufByteStream<reqwest::Error, S>
);

/// The possible errors that could occur when decoding an event from a stream.
#[derive(Debug)]
pub enum EventStreamError {
    EndOfStream,
    BadStreamFormat,
    BadEncoding(std::str::Utf8Error),
    BadData(serde_json::error::Error, Vec<u8>),
}

impl<S: Unpin + Stream<Item = Result<Bytes, reqwest::Error>>> EventStream<S> {

    /// Return the next event in the stream, or an error if the stream
    /// terminates or is malformed.
    pub async fn next(&mut self) -> Result<Event, EventStreamError> {
        let mut event_id:   Option<Vec<u8>> = None;
        let mut event_type: Option<Vec<u8>> = None;
        let mut raw_event_data = Vec::new();
        let mut started = false;
        loop {
            let line = self.0.read_line().await;
            match line.as_slice() {
                b"" => return Err(EventStreamError::EndOfStream),
                b":\n" => { /* heartbeat message */ },
                b"\n" =>
                    if started {
                        let event = Event {
                            id: event_id.map(|id| {
                                String::from_utf8_lossy(&id).trim().to_string()
                            }),
                            event: event_type.map(|event| {
                                String::from_utf8_lossy(&event).trim().to_string()
                            }),
                            data: match serde_json::from_slice(&raw_event_data) {
                                Ok(data) => data,
                                Err(err) => return Err(EventStreamError::BadData(err, raw_event_data)),
                            },
                        };
                        return Ok(event)
                    },
                line => {
                    started = true;
                    match line.splitn(2, |c| *c == b':').collect::<Vec<_>>().as_slice() {
                        [b"id", id] => {
                            if event_id.is_some() {
                                return Err(EventStreamError::BadStreamFormat);
                            } else {
                                event_id = Some(id.to_vec());
                            }
                        },
                        [b"event", event] => {
                            if event_type.is_some() {
                                return Err(EventStreamError::BadStreamFormat);
                            } else {
                                event_type = Some(event.to_vec());
                            }
                        },
                        [b"data", data] => {
                            raw_event_data.extend_from_slice(data);
                        },
                        _ => return Err(EventStreamError::BadStreamFormat),
                    }
                },
            }
        }
    }
}

#[tokio::main]
async fn main() {
    let client = reqwest::Client::new();
    let subscription =
        json!({
            "#container": {
                "mousemove": [
                    ".clientX",
                    ".clientY",
                    "window.innerHeight",
                    "window.innerWidth",
                ],
            },
            "window": {
                "resize": [
                    "window.innerHeight",
                    "window.innerWidth",
                ],
            },
        });

    let draw_body = |mouse: &Option<(u64, u64)>, window: &Option<(u64, u64)>| -> String {
        let mut angle;
        let saturation;
        let lightness;
        if let (Some((x, y)), Some((w, h))) = (mouse, window) {
            let x = *x as f64;
            let y = *y as f64;
            let w = *w as f64;
            let h = *h as f64;
            angle = (y - h / 2_f64).atan2(x - w / 2_f64).to_degrees() + 90.0;
            if angle < 0.0 { angle = angle + 360.0 }
            let ratio_from_edge = 1.0 -
                ((y - h / 2_f64).abs() + (x - w / 2_f64).abs())
                / (h / 2_f64 + w / 2_f64);
            saturation = 100.0 * ratio_from_edge;
            lightness = 100.0 - 50.0 * ratio_from_edge;
        } else {
            angle = 0_f64;
            saturation = 100_f64;
            lightness = 50.0;
        }
        format!("<div id=\"container\" style=\"overflow: hidden; margin: 0px; \
                 padding: 0px; height: 100vh; background:                     \
                 hsl({angle}, {saturation}%, {lightness}%);                   \
                 width: 100vw; text-align: center; position: relative;\">     \
                 <span style=\"position: absolute; top: 50%; transform:       \
                 translate(-50%, -50%) rotate({angle}deg); font-family:       \
                 Helvetica Neue; font-weight: 200; font-size: 250pt; color:   \
                 white; background: rgba(0, 0, 0, 0.4); border-radius: 300pt; \
                 border: none; padding: 100pt; width: 550pt; text-shadow:     \
                 0 0 25pt black\">{angle}°</span></div>",
                angle = angle as i64,
                saturation = saturation,
                lightness = lightness)
    };

    if let Ok(response) =
        client.post("http://localhost:1123/?subscribe")
        .body(subscription.to_string()).send().await {
            let mut events =
                EventStream(BufByteStream::from_result_stream(response.bytes_stream()));

            // The main event loop
            let mut mouse_location: Option<(u64, u64)> = None;
            let mut window_size:    Option<(u64, u64)> = None;
            loop {
                if let Err(err) =
                    client.post("http://localhost:1123/")
                    .body(draw_body(&mouse_location, &window_size))
                    .send().await {
                        println!("{:?}", err);
                        break;
                    }
                let event = match events.next().await {
                    Ok(event) => event,
                    Err(err) => {
                        println!("{:?}", err);
                        break;
                    }
                };
                // Dispatch on events to update the model
                if let Some(id) = event.id {
                    match id.as_str()  {
                        "#container" => {
                            mouse_location =
                                Some((event.data.get(".clientX").unwrap().as_u64().unwrap(),
                                      event.data.get(".clientY").unwrap().as_u64().unwrap()));
                            window_size =
                                Some((event.data.get("window.innerWidth").unwrap().as_u64().unwrap(),
                                      event.data.get("window.innerHeight").unwrap().as_u64().unwrap()));
                        },
                        "window" => {
                            window_size =
                                Some((event.data.get("window.innerWidth").unwrap().as_u64().unwrap(),
                                      event.data.get("window.innerHeight").unwrap().as_u64().unwrap()));
                        },
                        _ => { },
                    }
                }
            }

        } else {
            println!("Couldn't connect to myxine server.");
        }
}
