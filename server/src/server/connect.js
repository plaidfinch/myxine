let socket = new WebSocket("ws://" + location.host + location.pathname);
socket.onmessage = message => postMessage(JSON.parse(message.data));
onmessage = message => socket.send(JSON.stringify(message.data));
