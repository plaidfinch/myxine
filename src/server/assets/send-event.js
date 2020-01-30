let thisUrl; // Url to send things to, initialized by first message

onmessage = function(e) {
    let data = e.data;
    if (data.hasOwnProperty('thisUrl')) {
        thisUrl = data.thisUrl;
    } else {
        let request = new XMLHttpRequest();
        const url =
              thisUrl
              + "?event="  + encodeURIComponent(data.eventType)
              + "&target=" + encodeURIComponent(data.targetQuery)
              + "&id="     + encodeURIComponent(data.targetId);
        const jsonData = JSON.stringify(data.returnData);
        request.open("POST", url);
        request.setRequestHeader("Content-Type", "application/json");
        request.send(jsonData);
    }
};
