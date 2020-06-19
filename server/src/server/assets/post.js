onmessage = function(e) {
    let data = e.data;
    let request = new XMLHttpRequest();
    request.open("POST", data.url);
    request.setRequestHeader("Content-Type", data.contentType);
    request.send(data.data);
};
