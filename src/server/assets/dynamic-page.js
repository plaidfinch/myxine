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

    // Regular expression to determine if a string is the name of a standard
    // html element -- defined by scraping the contents of the reference at
    // https://developer.mozilla.org/en-US/docs/Web/HTML/Element, using this:
    // Array.from(document.querySelectorAll('article table.standard-table td:first-child code'))
    //     .map(e => e.innerText
    //          .replace('<', '')
    //          .replace('>', ''))
    //     .toString()
    //     .replace(/,/g, '|')
    const htmlElement =
          /^(html|base|head|link|meta|style|title|body|address|article|aside|footer|header|h1|h2|h3|h4|h5|h6|hgroup|main|nav|section|blockquote|dd|div|dl|dt|figcaption|figure|hr|li|main|ol|p|pre|ul|a|abbr|b|bdi|bdo|br|cite|code|data|dfn|em|i|kbd|mark|q|rb|rp|rt|rtc|ruby|s|samp|small|span|strong|sub|sup|time|u|var|wbr|area|audio|img|map|track|video|embed|iframe|object|param|picture|source|canvas|noscript|script|del|ins|caption|col|colgroup|table|tbody|td|tfoot|th|thead|tr|button|datalist|fieldset|form|input|label|legend|meter|optgroup|option|output|progress|select|textarea|details|dialog|menu|summary|slot|template|acronym|applet|basefont|bgsound|big|blink|center|command|content|dir|element|font|frame|frameset|image|isindex|keygen|listing|marquee|menuitem|multicol|nextid|nobr|noembed|noframes|plaintext|shadow|spacer|strike|tt|xmp)$/;

    // Get a list of objects by CSS queryAll selector or a path in the DOM
    // These should be unambiguous, provided that no top-level DOM object has a
    // name which is the same as a valid HTML tag name -- and this is indeed the
    // case in our pages
    function queryAll(q, all) {
        // Determine if we should match all, or just one
        if (typeof all === 'undefined') {
            all = true;
        }
        // Trim the query to remove spaces for matching below
        q = q.trim();
        // If the query starts with a valid HTML element or any non-word
        // character, assume it's a CSS selector
        if (/^\W/.test(q) || htmlElement.test(q.split(/\b/, 1)[0])) {
            try {
                if (all) {
                    return Array.from(document.querySelectorAll(q));
                } else {
                    return document.querySelector(q);
                }
            } catch(err) {
                debug("Selector lookup queryAll failed: '" + q + "': " + err);
                return [];
            }
        // Otherwise, assume it's an object lookup
        } else {
            const segments = q.split(".");
            let object = window;
            segments.forEach(segment => object = object[segment]);
            if (typeof object !== 'undefined') {
                if (all) {
                    return [object];
                } else {
                    return object;
                }
            } else {
                debug("Object lookup queryAll failed: '" + q + "'");
                if (all) {
                    return [];
                } else {
                    return null;
                }
            }
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

    function sendEvent(eventType, target, returnData) {
        let url = window.location.href
            + "?event="  + encodeURIComponent(eventType)
            + "&target=" + encodeURIComponent(targetQuery);
        sendEventWorker.postMessage({
            url: url,
            contentType: "application/json",
            data: JSON.stringify(returnData),
        });
    }

    // Actually send a query result back to the server
    let sendQueryResultsWorker = new Worker('/.myxine/assets/post.js');

    function sendQueryResults(id, results) {
        let url = window.location.href
            + "?query="  + encodeURIComponent(id);
        sendQueryResultsWorker.postMessage({
            url: url,
            contentType: "application/json",
            data: JSON.stringify({
                id: id,
                results: results,
            }),
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

    // TODO: Replace query interface with eval (using Function())?

    // Query the page for a particular list of values
    function query(event) {
        const id = event.id;
        const data = JSON.parse(event.data);
        const results = {};
        try {
            data.forEach(([target, properties]) => {
                results[target] = [];
                const object = queryAll(target, false);  // get just one
                properties.forEach(property => {
                    let value = object[property];
                    let type = typeof value;
                    // If it's not a primitive type, don't return it
                    if (!(type === 'boolean' || type === 'number' || type === 'string')) {
                        value = null;
                    }
                    results[target].push(value);
                });
            });
        } catch(err) {
            debug("Could not query page properly: " + err);
        }
        sendQueryResults(id, results);
    }

    const customJsonFormatters = {
        // string: x => x,
        // int: x => x,
        // bool: x => x,
        // double: x => x,
        // Add more here if there's a need to support more complex object types
    };

    // Parse a description of events and interfaces, and return a mapping from
    // event name to mappings from property name -> formatter for that property
    function parseEventDescriptions(enabledEvents) {
        let events = {};
        enabledEvents.forEach(([eventName, interfaceName]) => {
            events[eventName] = {};
            while (true) {
                const interface = enabledEvents.interfaces[interfaceName];
                const properties = Object.keys(interface.properties);
                for (let property in properties) {
                    let formatter = customJsonFormatters[property];
                    if (typeof formatter === 'undefined') {
                        formatter = (x => x);
                    }
                    events[eventName][property] = formatter;
                }
                if (interface.inherits !== null) {
                    interface = enabledEvents.interfaces[interface.inherits];
                } else {
                    break;
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
        events.forEach(([eventName, eventProperties]) => {
            window.addEventListener(eventName, event => {
                // Calculate the id path
                const path =
                      event.composedPath()
                      .filter(t => t instanceof Element)
                      .map(target => {
                          const pathElement = { tagName: target.tagName };
                          for (let i = target.attributes.length - 1; i >= 0; i--) {
                              const attribute = target.attributes[i];
                              const name = attribute.name;
                              const value = attribute.value;
                              pathElement[name] = value;
                          }
                          return pathElement;
                      });

                // Extract the relevant properties
                const data = {};
                events[eventName].forEach(([property, formatter]) => {
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
    r.onload = () => setupListeners(JSON.parse(r.responseText));
    r.open('GET', '/.myxine/assets/enabled-events.json');
    r.send();

    // Actually set up SSE...
    let sse = new EventSource(window.location.href + "?updates");
    sse.addEventListener("body", setBody);
    sse.addEventListener("clear-body", clearBody);
    sse.addEventListener("title", setTitle);
    sse.addEventListener("clear-title", clearTitle);
    sse.addEventListener("refresh", refresh);
    sse.addEventListener("queryAll", query);
}
