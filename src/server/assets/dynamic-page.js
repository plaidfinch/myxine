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

    // Get a list of objects by CSS query selector or a path in the DOM
    // These should be unambiguous, provided that no top-level DOM object has a
    // name which is the same as a valid HTML tag name -- and this is indeed the
    // case in our pages
    function query(q) {
        q = q.trim();
        // If the query starts with a valid HTML element or any non-word
        // character, assume it's a CSS selector
        if (/^\W/.test(q) || htmlElement.test(q.split(/\b/, 1)[0])) {
            try {
                return Array.from(document.querySelectorAll(q));
            } catch(err) {
                debug("Selector lookup query failed: '" + q + "': " + err);
                return [];
            }
        // Otherwise, assume it's an object lookup
        } else {
            const segments = q.split(".");
            let object = window;
            segments.forEach(segment => object = object[segment]);
            if (typeof object !== 'undefined') {
                return [object];
            } else {
                debug("Object lookup query failed: '" + q + "'");
                return [];
            }
        }
    }

    // Filter an object's properties for only primitives (removing null also)
    function primitiveProperties(object) {
        let result = {};
        for (let key in object) {
            let val = object[key];
            let type = typeof(val);
            if (type === 'boolean' || type === 'number' || type === 'string') {
                result[key] = val;
            }
        };
        return result;
    }

    // Actually send an event back to the server
    // This uses a web worker to avoid doing the sending work in the main thread
    let sendEventWorker = new Worker('/.myxine/assets/send-event.js');

    // Tell the worker where it'll be sending its messages...
    sendEventWorker.postMessage({thisUrl: window.location.href});
    function sendEvent(targetQuery, targetId, eventType, returnData) {
        sendEventWorker.postMessage({
            targetQuery: targetQuery,
            targetId: targetId,
            eventType: eventType,
            returnData: returnData,
        });
    }

    // Remove all event listeners we set, then set again from subscriptions
    function updateSubscription() {
        listeners.forEach(e => {
            debug("Removing listener: " + e.eventTarget + ": " + e.eventType);
            e.eventTarget.removeEventListener(e.eventType, e.eventListener, true);
        });
        listeners = [];
        debug("Setting up new subscription: " + JSON.stringify(subscription));
        Object.entries(subscription).forEach(([targetQuery, events]) => {
            debug("Processing target path: " + targetQuery);
            events.forEach(eventType => {
                debug("Processing event type: " + eventType);
                const targets = query(targetQuery); // All things matching query
                targets.forEach(eventTarget => {
                    const eventListener = (function (event) {
                        let returnData = primitiveProperties(event);
                        // Keep track of the current target id, so the user can
                        // make note of it if they want it. Potentially other
                        // properties of the currentTarget could be put here in
                        // the future, but we can't serialize the whole thing
                        // because it might be huge! It's also unlikely to be
                        // useful to put further info there, because the user
                        // can use the id to look up more things.
                        returnData.currentTarget = {};
                        let targetId = event.currentTarget.id;
                        if (typeof targetId === 'string') {
                            returnData.currentTarget.id = targetId;
                        }
                        sendEvent(targetQuery, targetId, eventType, returnData);
                    });
                    if (eventTarget !== null) {
                        listeners.push({
                            eventTarget: eventTarget,
                            eventType: eventType,
                            eventListener: eventListener,
                        });
                        debug("Adding event listener: " + eventTarget + ": " + eventType);
                        eventTarget.addEventListener(eventType, eventListener, true);
                    } else {
                        debug("Could not look up target path: " + targetQuery);
                    }
                });
            });
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
            updateSubscription();
        });
    }

    // These are the handlers for SSE events...
    function subscribe(event) {
        debug("Received new subscription: " + event.data);
        subscription = JSON.parse(event.data);
        updateSubscription();
    }

    // New body
    function setBody(event) {
        setBodyTo(event.data);
    }

    // New empty body
    function clearBody(event) {
        setBodyTo("");
        updateSubscription();
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
        location.reload();
    }

    // Actually set up SSE...
    let sse = new EventSource(window.location.href + "?updates");
    sse.addEventListener("body", setBody);
    sse.addEventListener("clear-body", clearBody);
    sse.addEventListener("title", setTitle);
    sse.addEventListener("clear-title", clearTitle);
    sse.addEventListener("refresh", refresh);
    sse.addEventListener("subscribe", subscribe);

    // Make sure the subscription gets updated once the whole page is loaded
    if (document.readyState === "loading") {
        document.addEventListener('readystatechange', () => {
            if (document.readyState === "interactive") {
                updateSubscription();
            }
        });
    } else {
        updateSubscription();
    }
}
