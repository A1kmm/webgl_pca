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

    function linearCombine(parts) {
        parts = NJS.fromList(parts);
        var accum = parts[0].values.map(function(x) { return x * parts[0].mup; });
        for (var i in parts) {
            if (i == 0) continue;
            for (var j in parts[i].values)
                accum[j] += parts[i].mup * parts[i].values[j];
        }
        return accum;
    }

    function trilinearInterpolate(f, xi) {
        var result = 0.0;
        xi = xi._0;
        var basisTable = [[1 - xi._0, xi._0], [1 - xi._1, xi._1], [1 - xi._2, xi._2]];
        for (var i = 0; i < 2; i++) {
            for (var j = 0; j < 2; j++) {
                for (var k = 0; k < 2; k++) {
                    result += basisTable[0][i] * basisTable[1][j] * basisTable[2][k] * f({ctor: "TrilinearLocalNode", _0: i + j * 2 + k * 4 + 1});
                }
            }
        }
        return result;
    }

    function bicubicLinearInterpolate(f, xi) {
        var result = 0.0;
        xi = xi._0;
        // console.log(xi);
        var xi0_2 = xi._0 * xi._0, xi0_3 = xi0_2 * xi._0,
            xi1_2 = xi._1 * xi._1, xi1_3 = xi1_2 * xi._1;
        var basisTable = [[1 - 3 * xi._0 + 3 * xi0_2 - xi0_3,
                           3 * xi._0 - 6 * xi0_2 + 3 * xi0_3,
                           3 * xi0_2 - 3 * xi0_3, xi0_3],
                          [1 - 3 * xi._1 + 3 * xi1_2 - xi1_3,
                           3 * xi._1 - 6 * xi1_2 + 3 * xi1_3,
                           3 * xi1_2 - 3 * xi1_3, xi1_3],
                          [1 - xi._2, xi._2]];
        for (var i = 0; i < 4; i++) {
            for (var j = 0; j < 4; j++) {
                for (var k = 0; k < 2; k++) {
                    result += basisTable[0][i] * basisTable[1][j] * basisTable[2][k] * f({ctor: "BicubicLinearLocalNode", _0: i + j * 4 + k * 16 + 1});
                }
            }
        }
        return result;
    }

    return elm.Native.FastJSON = { fastJSON: fastJSON,
                                   rawLookup: F2(rawLookup),
                                   send: send,
                                   simpleRemapArray: F2(simpleRemapArray),
                                   linearRemapArray: F2(linearRemapArray),
                                   bicubicLinearInterpolate: F2(bicubicLinearInterpolate),
                                   trilinearInterpolate: F2(trilinearInterpolate),
                                   linearCombine: linearCombine
                                 };
};
