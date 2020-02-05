export function activate(initialSubscription, diff, debugMode) {

    // The initial subscription at page load time
    let subscription = JSON.parse(initialSubscription);

    // The initial set of listeners is empty
    let listeners = [];

    // Current animation frame callback ID, if any
    let animationId = null;

    // Print debug info if in debug build mode
    function debug(string) {
        if (debugMode) {
            console.log(string);
        }
    }

    // NOTE: Why do we use separate workers for events and query results, but
    // only one worker for all events? Well, it matters that events arrive in
    // order so using one worker forces them to be linearized. And having a
    // second, separate worker for query results means that query results can be
    // processed concurrently with events, since their interleaving doesn't
    // matter.

    // Actually send an event back to the server
    let sendEventWorker = new Worker('/.myxine/assets/post.js');

    function sendEvent(type, path, properties) {
        let url = window.location.href + '?event';
        let data = JSON.stringify({
            event: type,
            id: path,
            data: properties,
        });
        debug("Sending event: " + data);
        sendEventWorker.postMessage({
            url: url,
            contentType: "application/json",
            data: data,
        });
    }

    // Actually send a query result back to the server
    let sendEvalResultWorker = new Worker('/.myxine/assets/post.js');

    function sendEvalResult(id, result) {
        let url = window.location.href
            + "?result="  + encodeURIComponent(id);
        sendEvalResultWorker.postMessage({
            url: url,
            contentType: "application/json",
            data: JSON.stringify(result),
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
            diff.innerHTML(document.body, body);
        });
    }

    // These are the handlers for SSE events...
    function subscribe(event) {
        debug("Received new subscription: " + event.data);
        subscription = JSON.parse(event.data);
    }

    // New body
    function setBody(event) {
        setBodyTo(event.data);
    }

    // New empty body
    function clearBody(event) {
        setBodyTo("");
    }

    // New title
    function setTitle(event) {
        document.title = event.data;
    }

    // New empty title
    function clearTitle(event) {
        document.title = "";
    }

    // Reload the *whole* page from the server
    // Called when transitioning to static page, among other situations
    function refresh(event) {
        window.location.reload();
    }

    // Evaluate a JavaScript expression in the global environment
    function evaluate(expression) {
        // TODO: LRU-limited memoization (using memoizee?)
        return Function('return ' + expression)();
    }

    // Evaluate a JavaScript expression and return the result
    function evaluateAndRespond(event) {
        const result = evaluate(event.data);
        sendEvalResult(event.id, result);
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
        Object.entries(enabledEvents.events).forEach(([eventName, eventInfo]) => {
            // Accumulate the desired fields for the event into a map from field
            // name to formatter for the objects in that field
            debug(eventInfo);
            let interfaceName = eventInfo['interface']; // most specific interface
            let theInterface = enabledEvents.interfaces[interfaceName];
            events[eventName] = {};
            while (true) {
                debug(theInterface);
                const properties = Object.keys(theInterface.properties);
                properties.forEach(property => {
                    let formatter = customJsonFormatters[property];
                    if (typeof formatter === 'undefined') {
                        formatter = (x => x); // Default formatter is identity
                    }
                    if (typeof events[eventName][property] === 'undefined') {
                        events[eventName][property] = formatter;
                    } else {
                        debug("Duplicate property in " + eventName + ": " + property);
                    }
                });
                if (theInterface.inherits !== null) {
                    // Check ancestors for more fields to add
                    theInterface = enabledEvents.interfaces[theInterface.inherits];
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
    function setupListeners(events) {
        // Set up event handlers
        Object.entries(events).forEach(([eventName, eventInfo]) => {
            window.addEventListener(eventName, event => {
                // Calculate the id path
                const path =
                      event.composedPath()
                      .filter(t => t instanceof Element)
                      .map(target => {
                          const pathElement = {
                              tagName: target.tagName,
                              attributes: {},
                          };
                          for (let i = target.attributes.length - 1; i >= 0; i--) {
                              const attribute = target.attributes[i];
                              const name = attribute.name;
                              const value = attribute.value;
                              pathElement.attributes[name] = value;
                          }
                          return pathElement;
                      });

                // Extract the relevant properties
                const data = {};
                Object.entries(events[eventName]).forEach(([property, formatter]) => {
                    data[property] = formatter(event[property]);
                });
                // Send the event back to the server
                // TODO: implement batching
                sendEvent(eventName, path, data);
            });
        });

    }

    // Fetch the description of the events we wish to support
    const r = new XMLHttpRequest();
    r.onerror = () => debug("Could not fetch list of enabled events!");
    r.onload = () => {
        const enabledEvents = JSON.parse(r.responseText);
        debug(enabledEvents);
        setupListeners(parseEventDescriptions(enabledEvents));
    };
    r.open('GET', '/.myxine/assets/enabled-events.json');
    r.send();

    // Actually set up SSE...
    let sse = new EventSource(window.location.href + "?updates");
    sse.addEventListener("body", setBody);
    sse.addEventListener("clear-body", clearBody);
    sse.addEventListener("title", setTitle);
    sse.addEventListener("clear-title", clearTitle);
    sse.addEventListener("refresh", refresh);
    sse.addEventListener("evaluate", evaluateAndRespond);
}
