Elm.Native.JQuery = function(elm) {
    "use strict";
    elm.Native = elm.Native || {};
    if (elm.Native.JQuery) return elm.Native.JQuery;
    
    var count = 0;

    var jqueryUsers = [];

    function subscribejQueryReady(f) {
        jqueryUsers.push(f);
    }

    function jqueryLoaded() {
        count++;
        if (count != 2)
            return;
        for (var i in jqueryUsers)
            jqueryUsers[i]();
    }

    var cse = document.createElement("link");
    cse.setAttribute("rel", "stylesheet");
    cse.setAttribute("type", "text/css");
    cse.setAttribute("href", "http://code.jquery.com/ui/1.10.3/themes/ui-lightness/jquery-ui.min.css");
    document.head.appendChild(cse);

    var scr = document.createElement("script");
    scr.setAttribute("src", "http://code.jquery.com/jquery-1.10.2.min.js");
    scr.setAttribute("type", "text/javascript");
    scr.onload = jqueryLoaded;
    document.head.appendChild(scr);

    scr = document.createElement("script");
    scr.setAttribute("src", "http://code.jquery.com/ui/1.10.3/jquery-ui.min.js");
    scr.setAttribute("type", "text/javascript");
    scr.onload = jqueryLoaded;
    document.head.appendChild(scr);

    return elm.Native.JQuery = { subscribejQueryReady: subscribejQueryReady };
}
