export function activate(initialFrameId, initialSubscription, debugMode) {

    // Print debug info if in debug build mode
    const debug = (debugMode ? console.log : function () { /* do nothing */ });

    // The initial set of listeners is empty
    let listeners = {};

    // Current animation frame callback ID, if any
    let animationId = null;

    // The global list of all events and their properties
    let allEventDescriptions;

    // The current frame ID, to be updated each time an update event comes in
    let currentFrameId = initialFrameId;
    // The *visible* frame ID, which is updated only after each body redraw
    let visibleFrameId = initialFrameId;
    debug("Initial frame id:", currentFrameId);

    // NOTE: Why do we use separate workers for events and query results, but
    // only one worker for all events? Well, it matters that events arrive in
    // order so using one worker forces them to be linearized. And having a
    // second, separate worker for query results means that query results can be
    // processed concurrently with events, since their interleaving doesn't
    // matter.

    // Actually send an event back to the server
    let sendEventWorker = new window.Worker("/.myxine/assets/post.js");

    function sendEvent(frameId, type, path, properties) {
        let url = window.location.href
            + "?page-event&page-frame=" + encodeURIComponent(frameId);
        let data = JSON.stringify({
            event: type,
            targets: path,
            properties: properties
        });
        debug("Sending event:", data);
        sendEventWorker.postMessage({
            url: url,
            contentType: "application/json",
            data: data
        });
    }

    // Actually send a query result back to the server
    let sendEvalResultWorker = new window.Worker("/.myxine/assets/post.js");

    function sendEvalResult(id, result) {
        let url = window.location.href
            + "?page-result="  + encodeURIComponent(id);
        sendEvalResultWorker.postMessage({
            url: url,
            contentType: "application/json",
            data: JSON.stringify(result),
        });
    }

    function sendEvalError(id, error) {
        let url = window.location.href
            + "?page-error="  + encodeURIComponent(id);
        sendEvalResultWorker.postMessage({
            url: url,
            contentType: "text/plain",
            data: error.toString()
        });
    }

    // Set the body
    function setBodyTo(body) {
        // Cancel the previous animation frame, if any
        if (animationId !== null) {
            window.cancelAnimationFrame(animationId);
        }
        // Redraw the body before the next repaint (but not right now yet)
        animationId = window.requestAnimationFrame(timestamp => {
            window.diff.innerHTML(document.body, body);
            visibleFrameId = currentFrameId;
            // only now we set the visibleFrameId, because we've just updated
            // the body to match the currentFrameId
        });
    }

    // These are the handlers for SSE events...
    function resubscribe(event) {
        debug("Received new subscription:", event.data);
        const subscription = JSON.parse(event.data);
        Object.entries(listeners).forEach(([eventName, listener]) => {
            debug("Removing listener:", eventName);
            window.removeEventListener(eventName, listener);
        });
        listeners = {};
        setupListeners(subscription);
    }

    // Wrap an event handler so that it sets the current frame ID when it fires
    function settingFrameId(f) {
        return function(event) {
            if (typeof event.lastEventId !== "undefined"
                && event.lastEventId !== "") {
                currentFrameId = event.lastEventId;
            } else {
                debug("Couldn't set current frame id during:", f);
            }
            return f(event);
        };
    }

    // Changing the DOM contents
    function setBody(event)    { setBodyTo(event.data); }
    function clearBody(event)  { setBodyTo(""); }
    function setTitle(event)   { document.title = event.data; }
    function clearTitle(event) { document.title = ""; }
    function newFrame(event)   { visibleFrameId = currentFrameId;  }

    // Reload the *whole* page from the server
    // Called when transitioning to static page, among other situations
    function refresh(event) {
        window.location.reload();
    }

    // The evaluate-* type events should **NOT** set the current frame ID
    // This is because they are "global" and not frame-bound

    // Evaluate a JavaScript expression in the global environment
    function evaluate(expression, statementMode) {
        // TODO: LRU-limited memoization (using memoizee?)
        if (!statementMode) {
            return Function("return (" + expression + ")")();
        } else {
            return Function(expression)();
        }
    }

    // Evaluate a JavaScript expression and return the result
    function evaluateAndRespond(statementMode, event) {
        debug("Evaluating expression '" + event.data + "'(id "
              + event.lastEventId + ") as a"
              + (statementMode ? " statement" : "n expression"));
        try {
            let result = evaluate(event.data, statementMode);
            if (typeof result === "undefined") {
                result = null;
            }
            debug("Sending back result response (id "
                  + event.lastEventId
                  + "):", result);
            sendEvalResult(event.lastEventId, result);
        } catch(err) {
            debug("Sending back error response (id "
                  + event.lastEventId
                  + "):", err);
            sendEvalError(event.lastEventId, err);
        }
    }

    // Functions from JavaScript objects to serializable objects, keyed by the
    // types of those objects as represented in the interface description
    const customJsonFormatters = {
        // Add here if there's a need to support more complex object types
    };

    // Parse a description of events and interfaces, and return a mapping from
    // event name to mappings from property name -> formatter for that property
    function parseEventDescriptions(enabledEvents) {
        let events = {};
        const allEvents = enabledEvents.events;
        Object.entries(allEvents).forEach(([eventName, eventInfo]) => {
            // Accumulate the desired fields for the event into a map from
            // field name to formatter for the objects in that field
            let interfaceName = eventInfo["interface"]; // most specific
            let theInterface = enabledEvents.interfaces[interfaceName];
            events[eventName] = {};
            while (true) {
                const properties = Object.keys(theInterface.properties);
                properties.forEach(property => {
                    let formatter = customJsonFormatters[property];
                    if (typeof formatter === "undefined") {
                        formatter = (x => x); // Default formatter is id
                    }
                    if (typeof events[eventName][property] === "undefined") {
                        events[eventName][property] = formatter;
                    } else {
                        debug("Duplicate property in "
                              + eventName
                              + ": "
                              + property);
                    }
                });
                if (theInterface.inherits !== null) {
                    // Check ancestors for more fields to add
                    theInterface =
                        enabledEvents.interfaces[theInterface.inherits];
                } else {
                    break; // Top of interface hierarchy
                }
            }
        });
        return events;
    }

    // Given a mapping from events -> mappings from properties -> formatters for
    // those properties, set up listeners for all those events which send back
    // the appropriately formatted results when they fire
    function setupListeners(subscription) {
        if (subscription === null) {
            // Client requested universal subscription, so turn on all events
            subscription = Object.keys(allEventDescriptions);
        }
        // Set up event handlers
        subscription.forEach(eventName => {
            if (typeof allEventDescriptions[eventName] !== "undefined") {
                const listener = event => {
                    // Calculate the id path
                    const path =
                        event.composedPath()
                        .filter(t => t instanceof Element)
                        .map(target => {
                            const pathElement = {
                                tagName: target.tagName.toLowerCase(),
                                attributes: {},
                            };
                            const numAttrs = target.attributes.length;
                            for (let i = numAttrs - 1; i >= 0; i--) {
                                const attribute = target.attributes[i];
                                const name = attribute.name;
                                const value = attribute.value;
                                pathElement.attributes[name] = value;
                            }
                            return pathElement;
                        });

                    // Extract the relevant properties
                    const data = {};
                    Object.entries(allEventDescriptions[eventName])
                        .forEach(([property, formatter]) => {
                            data[property] = formatter(event[property]);
                        });
                    sendEvent(visibleFrameId, eventName, path, data);
                };
                debug("Adding listener:", eventName);
                window.addEventListener(eventName, listener);
                listeners[eventName] = listener;
            } else {
                debug("Invalid event name:", eventName);
            }
        });
    }

    // Fetch the description of the events we wish to support
    const r = new XMLHttpRequest();
    r.onerror = () => debug("Could not fetch list of enabled events!");
    r.onload = () => {
        const enabledEvents = JSON.parse(r.responseText);
        debug(enabledEvents);
        allEventDescriptions = parseEventDescriptions(enabledEvents);
        setupListeners(initialSubscription);
    };
    r.open("GET", "/.myxine/assets/enabled-events.json");
    r.send();

    // Actually set up SSE...
    let sse = new window.EventSource(window.location.href + "?updates");
    sse.onerror = () => {
        // When we disconnect from the server, stop trying to send it events
        // until we reconnect and receive a new subscription
        setupListeners([]);
        // Set up retry interval to attempt reconnection
        window.setTimeout(() => {
            sse = new window.EventSource(window.location.href + "?updates");
        }, 500); // half a second between retries
    };

    // The listeners:
    sse.addEventListener("body",        settingFrameId(setBody));
    sse.addEventListener("clear-body",  settingFrameId(clearBody));
    sse.addEventListener("title",       settingFrameId(setTitle));
    sse.addEventListener("clear-title", settingFrameId(clearTitle));
    sse.addEventListener("frame",       settingFrameId(newFrame));
    sse.addEventListener("refresh",     refresh);
    sse.addEventListener("subscribe",   resubscribe);
    sse.addEventListener("evaluate",    event => evaluateAndRespond(false, event));
    sse.addEventListener("run",         event => evaluateAndRespond(true, event));
}
