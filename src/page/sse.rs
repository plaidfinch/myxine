use hyper_usse;
use bytes::Bytes;
use futures::Future;
use tokio::sync::{mpsc, oneshot};

/// An SSE server implementing buffering, so "bursty" events can be sent without
/// lagging from the sender.
#[derive(Clone, Debug)]
pub struct BufferedServer {
    commands: mpsc::Sender<Command>,
}

pub enum Command {
    Connections(oneshot::Sender<usize>),
    SendToClients(Bytes, oneshot::Sender<usize>),
    SendHeartbeat(oneshot::Sender<usize>),
    DisconnectAll,
    AddClient(hyper::body::Sender),
}

impl BufferedServer {
    pub async fn new(buffer_size: usize) -> BufferedServer {
        let (commands, mut receiver) = mpsc::channel(buffer_size);
        tokio::spawn(async move {
            let mut server = hyper_usse::Server::new();
            while let Some(command) = receiver.recv().await {
                match command {
                    Command::SendHeartbeat(ret) => {
                        ret.send(server.send_heartbeat().await).unwrap_or(());
                    },
                    Command::SendToClients(bytes, ret) => {
                        ret.send(server.send_to_clients(bytes).await).unwrap_or(());
                    },
                    Command::Connections(ret) => {
                        ret.send(server.connections()).unwrap_or(());
                    },
                    Command::AddClient(sender) =>
                        server.add_client(sender),
                    Command::DisconnectAll =>
                        server.disconnect_all(),
                }
            }
        });
        BufferedServer{commands}
    }

    pub async fn add_client(&mut self, client: hyper::body::Sender) {
        self.commands.send(Command::AddClient(client)).await.unwrap_or(())
    }

    pub async fn send_to_clients<B: Into<Bytes>>(&mut self, text: B) -> impl Future<Output = usize> {
        let (sender, receiver) = oneshot::channel();
        self.commands.send(Command::SendToClients(text.into(), sender)).await.unwrap_or(());
        async { receiver.await.expect("oneshot::Sender dropped before sending \
                                       response from BufferedServer, which \
                                       should be impossible") }
    }

    pub async fn send_heartbeat(&mut self) -> impl Future<Output = usize> {
        let (sender, receiver) = oneshot::channel();
        self.commands.send(Command::SendHeartbeat(sender)).await.unwrap_or(());
        async { receiver.await.expect("oneshot::Sender dropped before sending \
                                       response from BufferedServer, which \
                                       should be impossible") }
    }

    #[allow(unused)]
    pub async fn disconnect_all(&mut self) {
        self.commands.send(Command::DisconnectAll).await.unwrap_or(())
    }

    pub async fn connections(&mut self) -> usize {
        let (sender, receiver) = oneshot::channel();
        self.commands.send(Command::Connections(sender)).await.unwrap_or(());
        receiver.await.expect("oneshot::Sender dropped before sending \
                               response from BufferedServer, which \
                               should be impossible")
    }
}
