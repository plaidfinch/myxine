export function activate(thisUrl, initialSubscription, innerHTML, debugMode) {
    // The initial subscription at page load time
    let subscription = JSON.parse(initialSubscription);
    // The new body, cached before it's put in place
    let body = "";
    // The initial set of listeners is empty
    let listeners = [];
    // Print debug info if in debug build mode
    function debug(string) {
        if (debugMode) {
            console.log(string);
        }
    }
    // Get a path starting at the given root element, or if given an id
    // like '#foo', from anywhere in the document
    function getPathFrom(path, root) {
        if (path.length > 0 && path[0] === "#") {
            return document.getElementById(path.substring(1));
        } else {
            const segments = path.split(".");
            let object = root;
            segments.forEach(segment => object = object[segment]);
            return object;
        }
    }
    // Get either an element by id '#foo' or an absolute path in the DOM
    function getAbsolutePath(path) {
        return getPathFrom(path, globalThis);
    }
    // If the path begins with '.', index into event, otherwise globally
    function getReturnDatum(path, event) {
        if (path.length > 0 && path[0] == ".") {
            return getPathFrom(path.substring(1), event);
        } else {
            return getAbsolutePath(path);
        }
    }
    // Actually send an event back to the server
    function sendEvent(targetPath, eventType, returnData) {
        let request = new XMLHttpRequest();
        let targetIdString;
        if (targetPath.length > 0 && targetPath[0] === "#") {
            targetIdString = "id=" + targetPath.substring(1);
        } else {
            targetIdString = "path=" + targetPath;
        }
        const url = thisUrl + "?event=" + eventType + "&" + targetIdString;
        const jsonData = JSON.stringify(returnData);
        request.open("POST", url);
        request.setRequestHeader("Content-Type", "application/json");
        request.addEventListener('load', () => {
            debug("Sent event to: " + url + ": " + jsonData);
        });
        request.send(jsonData);
    }
    // Remove all event listeners we set, then set again from subscriptions
    function updateSubscription() {
        listeners.forEach(e => {
            debug("Removing listener: " + e.eventTarget + ": " + e.eventType);
            e.eventTarget.removeEventListener(e.eventType, e.eventListener, true);
        });
        listeners = [];
        debug("Setting up new subscription: " + JSON.stringify(subscription));
        Object.entries(subscription).forEach(([targetPath, events]) => {
            debug("Processing target path: " + targetPath);
            Object.entries(events).forEach(([eventType, returnPaths]) => {
                debug("Processing event type: " + eventType);
                const eventListener = (function (event) {
                    var returnData = {};
                    returnPaths.forEach(returnPath => {
                        debug("Getting return path: " + returnPath);
                        let returnDatum = getReturnDatum(returnPath, event);
                        if (returnDatum !== null) {
                            returnData[returnPath] = returnDatum;
                        } else {
                            debug("Could not look up return path: " + returnPath);
                        }
                    });
                    sendEvent(targetPath, eventType, returnData);
                });
                const eventTarget = getAbsolutePath(targetPath);
                if (eventTarget !== null) {
                    listeners.push({
                        eventTarget: eventTarget,
                        eventType: eventType,
                        eventListener: eventListener,
                    });
                    debug("Adding event listener: " + eventTarget + ": " + eventType);
                    eventTarget.addEventListener(eventType, eventListener, true);
                } else {
                    debug("Could not look up target path: " + targetPath);
                }
            });
        });
    }
    // When the document is loaded, register all the subscriptions
    if (document.readyState != 'complete') {
        window.addEventListener('load', event => {
            debug("First load of event subscriptions.");
            updateSubscription();
        });
    }
    // Set the body using DiffHTML
    function setBodyTo(string) {
        body = string;
        setTimeout(() => {
            innerHTML(document.body, body);
        });
    }
    // These are the handlers for SSE events...
    function subscribe(event) {
        debug("Received new subscription: " + event.data);
        subscription = JSON.parse(event.data);
        updateSubscription();
    }
    function setBody(event) {
        setBodyTo(event.data);
        updateSubscription();
    }
    function clearBody(event) {
        setBodyTo("");
        updateSubscription();
    }
    function setTitle(event) {
        document.title = event.data;
    }
    function clearTitle(event) {
        document.title = "";
    }
    function refresh(event) {
        location.reload();
    }
    // Actually set up SSE...
    let sse = new EventSource(thisUrl + "?updates");
    sse.addEventListener("body", setBody);
    sse.addEventListener("clear-body", clearBody);
    sse.addEventListener("title", setTitle);
    sse.addEventListener("clear-title", clearTitle);
    sse.addEventListener("refresh", refresh);
    sse.addEventListener("subscribe", subscribe);
}
