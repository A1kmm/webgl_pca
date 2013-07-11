Elm.Native.Graphics.Slider = function(elm) {
    "use strict";

    elm.Native = elm.Native || {};
    elm.Native.Graphics = elm.Native.Graphics || {};

    if (elm.Native.Graphics.Slider != null)
        return elm.Native.Graphics.Slider;

    var Signal = Elm.Signal(elm);
    var newElement = Elm.Graphics.Element(elm).newElement;
    var Tuple2 = Elm.Native.Utils(elm).Tuple2;

    var pendingSliders = [];
    var jqReady = false;

    function jqready() {
        jqReady = true;
        processSliderQueue();
    }

    function processSliderQueue() {
        if (!jqReady)
            return;
        for (var i in pendingSliders) {
            var jqel = $(pendingSliders[i].element);
            if (pendingSliders[i].events != null) {
                (function() {
                    var events = pendingSliders[i].events.id;
                    pendingSliders[i].model.slide = function(e, ui) {
                        elm.notify(events, ui.value);
                    }
                })();
            }
            jqel.slider(pendingSliders[i].model);
        }
        pendingSliders = [];
    }

    var jq = Elm.Native.JQuery(elm);
    jq.subscribejQueryReady(jqready);

    function renderSlider(events) {
        return function (model) {
            var slider = document.createElement('div');
            pendingSliders.push({model: model, element: slider, events: events });
            processSliderQueue();
            return slider;
        }
    }

    function updateSlider(slider, oldModel, newModel) {
        if (oldModel == newModel)
            return;
        var changeModel = {};
        if (oldModel.min != newModel.min)
            changeModel.min = newModel.min;
        if (oldModel.max != newModel.max)
            changeModel.max = newModel.max;
        if (oldModel.step != newModel.step)
            changeModel.step = newModel.step;
        if (oldModel.value != newModel.value)
            changeModel.value = newModel.value;
        pendingSliders.push({model: changeModel, element: slider});
        processSliderQueue();
        return false;
    }

    function slider(w,h,min,max,step,defaultValue) {
        var events = Signal.constant(defaultValue);
        var elem = A3(newElement, w, h, {
            ctor: 'Custom',
            type: 'Slider',
            render: renderSlider(events),
            update: updateSlider,
            props: {},
            model: {w:w, h:h, min: min, max: max, step: step, value: defaultValue }
                  });
        return Tuple2(Signal.constant(elem), events);
    }

    return elm.Native.Graphics.Slider = { slider: F6(slider) };
}
