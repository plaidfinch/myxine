onmessage = function(e) {
    let request = new XMLHttpRequest();
    let targetIdString;
    if (e.targetPath.length > 0 && e.targetPath[0] === "#") {
        targetIdString = "id=" + e.targetPath.substring(1);
    } else {
        targetIdString = "path=" + e.targetPath;
    }
    const url = e.thisUrl + "?event=" + e.eventType + "&" + targetIdString;
    const jsonData = JSON.stringify(e.returnData);
    request.open("POST", url);
    request.setRequestHeader("Content-Type", "application/json");
    request.send(jsonData);
}
