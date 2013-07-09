Elm.Native.FastJSON = function(elm) {
    "use strict";

    elm.Native = elm.Native || {};
    if (elm.Native.FastJSON) return elm.Native.FastJSON;

    var NJS = Elm.Native.JavaScript(elm);
    var JS = Elm.JavaScript(elm);
    var List = Elm.List(elm);
    var Signal = Elm.Signal(elm);

    /* The follow code is just like Native.Http except that it keeps everything
     * as a JSString.
     */
    function registerReq(queue,responses) { return function(req) {
        if (req.url.ctor !== '[]') { sendReq(queue,responses,req); }
    };
                                          }

    function updateQueue(queue,responses) {
        if (queue.length > 0) {
            elm.notify(responses.id, queue[0].value);
            if (queue[0].value.ctor !== 'Waiting') {
                queue.shift();
                setTimeout(function() { updateQueue(queue,responses); }, 0);
            }
        }
    }

    function setHeader(pair) {
        request.setRequestHeader( JS.fromString(pair._0), JS.fromString(pair._1) );
    }

    function sendReq(queue,responses,req) {
        var response = { value: { ctor:'Waiting' } };
        queue.push(response);

        var request = null;
        if (window.ActiveXObject)  { request = new ActiveXObject("Microsoft.XMLHTTP"); }
        if (window.XMLHttpRequest) { request = new XMLHttpRequest(); }
        request.onreadystatechange = function(e) {
            if (request.readyState === 4) {
                response.value = (request.status >= 200 && request.status < 300 ?
                                  { ctor:'Success', _0:(request.responseText) } :
                                  { ctor:'Failure', _0:request.status, _1:JS.toString(request.statusText) });
                setTimeout(function() { updateQueue(queue,responses); }, 0);
            }
        };
        request.open(JS.fromString(req.verb), JS.fromString(req.url), true);
        List.map(setHeader)(req.headers);
        request.send(JS.fromString(req.body));
    }

    function send(requests) {
        var responses = Signal.constant(elm.Http.Waiting);
        var sender = A2( Signal.lift, registerReq([],responses), requests );
        function f(x) { return function(y) { return x; } }
        return A3( Signal.lift2, f, responses, sender );
    }

    function fastJSON(str) {
        return window.JSON.parse(str);
    }

    function rawLookup(arr, idx) {
        return arr[idx];
    }

    function simpleRemapArray(remap, glob) {
        var loc = [];
        for (var i in remap)
            loc[remap[i].loc - 1] = glob[remap[i].glob - 1];
        return loc;
    }

    function linearRemapArray(remap, glob) {
        var loc = [];
        for (var i in remap) {
            var newloc = 0.0;
            for (var j in remap[i].globs)
                newloc += remap[i].globs[j].mup * glob[remap[i].globs[j].glob - 1];
            glob[remap[i].glob]
            loc[remap[i].loc - 1] = newloc;
        }
        return loc;
    }

    return elm.Native.FastJSON = { fastJSON: fastJSON,
                                   rawLookup: F2(rawLookup),
                                   send: send,
                                   simpleRemapArray: F2(simpleRemapArray),
                                   linearRemapArray: F2(linearRemapArray) };
};
