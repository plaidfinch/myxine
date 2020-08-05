function myxine(enabledEvents) {
    // Print debug info if the user sets window.myxine = true
    window.myxine = { "debug": false };
    function debug(...args) {
        if (window.myxine.debug === true) {
            console.log(...args);
        }
    }

    // Current animation frame callback ID, if any
    let animationId = null;

    // Set the body
    function setBodyTo(body) {
        // Cancel the previous animation frame, if any
        if (animationId !== null) {
            window.cancelAnimationFrame(animationId);
        }
        // Redraw the body before the next repaint (but not right now yet)
        animationId = window.requestAnimationFrame(timestamp => {
            window.diff.innerHTML(document.body, body);
        });
    }

    // Evaluate a JavaScript expression in the global environment
    function evaluate(expression, statementMode) {
        if (!statementMode) {
            return Function("return (" + expression + ");")();
        } else {
            return Function(expression)();
        }
    }

    // Evaluate a JavaScript expression and return the result
    function evaluateAndRespond(statementMode, expression, id, worker) {
        debug("Evaluating expression '" + expression + "' as a"
              + (statementMode ? " statement" : "n expression"));
        try {
            let result = evaluate(expression, statementMode);
            if (typeof result === "undefined") {
                result = null;
            }
            debug("Sending back result response:", result);
            worker.postMessage({
                type: "evalResult",
                id: id,
                result: { Ok: result }
            })
        } catch(err) {
            debug("Sending back error response:", err);
            worker.postMessage({
                type: "evalResult",
                id: id,
                result: { Err: err.toString() }
            })
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

    // We only should set up page event listeners once
    let pageEventListenersSet = false;

    // Set up listeners for all those events which send back the appropriately
    // formatted results when they fire
    function setupPageEventListeners(worker) {
        if (!pageEventListenersSet) {
            pageEventListenersSet = true;
            const descriptions = parseEventDescriptions(enabledEvents);
            const subscription = Object.keys(descriptions);
            // Set up event handlers
            subscription.forEach(eventName => {
                if (typeof descriptions[eventName] !== "undefined") {
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
                        Object.entries(descriptions[eventName])
                            .forEach(([property, formatter]) => {
                                data[property] = formatter(event[property]);
                            });

                        // Send the event to the server
                        worker.postMessage({
                            type: "event",
                            event: eventName,
                            targets: path,
                            properties: data,
                        });
                    };
                    debug("Adding listener:", eventName);
                    window.addEventListener(eventName, listener);
                } else {
                    debug("Invalid event name:", eventName);
                }
            });
        }
    }

    // The handlers for events coming from the server:
    function setupServerEventListeners(worker) {
        worker.onmessage = workerMessage => {
            setupPageEventListeners(worker);
            let message = workerMessage.data;
            if (message !== null) {  // Worker will send null message to signal initialization.
                debug("Received message:", message);
                if (message.type === "reload") {
                    window.location.reload();
                } else if (message.type === "evaluate") {
                    evaluateAndRespond(message.statementMode, message.script, message.id, worker);
                } else if (message.type === "update") {
                    document.title = message.title;
                    if (message.diff) {
                        setBodyTo(message.body);
                    } else {
                        document.body.innerHTML = message.body;
                    }
                }
            }
        }
    }

    // When things are loaded, activate the page.
    window.addEventListener("load", () => {
        let worker = new Worker(location.href + "?connect")
        setupServerEventListeners(worker);
    });
}
