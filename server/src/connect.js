let socket = new WebSocket("ws://" + location.host + location.pathname);
socket.onmessage = message => postMessage(JSON.parse(message.data));
socket.onopen = () => postMessage(null); // Signal initialization success.
onmessage = message => {
    try {
        socket.send(JSON.stringify(message.data));
    } catch {
        // If the connection is dead, drop the event.
    }
}
