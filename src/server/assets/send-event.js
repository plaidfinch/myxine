onmessage = function(e) {
    let data = e.data;
    let request = new XMLHttpRequest();
    let targetIdString;
    if (data.targetPath.length > 0 && data.targetPath[0] === "#") {
        targetIdString = "id=" + data.targetPath.substring(1);
    } else {
        targetIdString = "path=" + data.targetPath;
    }
    const url = data.thisUrl + "?event=" + data.eventType + "&" + targetIdString;
    const jsonData = JSON.stringify(data.returnData);
    request.open("POST", url);
    request.setRequestHeader("Content-Type", "application/json");
    request.send(jsonData);
};
