// Run this script on https://developer.mozilla.org/en-US/docs/Web/Events and it
// will generate a listing of which web standard events are documented to
// bubble, and which are not. This listing is saved in bubbles.json. Note that
// because this script uses async XMLHttpRequests, you need to manually wait for
// the terminal reports of XHRs to quiesce before reading out the value of the
// results and errors, since they're filled in asynchronously after the body of
// the function is done running.

function scrapeBubbling() {
    let results = {};
    let errors = {};
    Array.from(document.querySelectorAll('#Standard_events ~ p + div tr td:first-child a'))
        .map(a => {
            const request = new XMLHttpRequest();
            request.addEventListener("load", () => {
                try {
                    let bubbles = Array.from(request.responseXML.querySelectorAll('table.properties tr'))
                        .filter(e => e.querySelector('th') !== null ? e.querySelector('th').innerText === 'Bubbles' : false)[0]
                        .querySelector('td').innerText === 'Yes';
                    console.log(a.innerText, bubbles);
                    let item = results[a.innerText];
                    if (typeof item === 'undefined') {
                        item = {};
                    }
                    item[a.href] = bubbles;
                    results[a.innerText] = item;
                } catch(err) {
                    errors[a.href] = a.innerText;
                }
            });
            request.open("GET", a.href);
            request.responseType = "document";
            request.send();
        });
    return [results, errors];
}

[r, e] = scrapeBubbling();
