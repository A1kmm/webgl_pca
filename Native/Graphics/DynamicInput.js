Elm.Native.Graphics.DynamicInput = function(elm) {
    "use struct";
    elm.Native = elm.Native || {};
    elm.Native.Graphics = elm.Native.Graphics || {};
    if (elm.Native.Graphics.DynamicInput) return elm.Native.Graphics.DynamicInput;

    var newNode = ElmRuntime.use(ElmRuntime.Render.Utils).newElement;
    var newElement = Elm.Graphics.Element(elm).newElement;
    var Utils = Elm.Native.Utils(elm);
    var Tuple2 = Utils.Tuple2;
    var JS = Elm.JavaScript(elm);
    var Signal = Elm.Signal(elm);
    
    function dynamicDropDown(defText, defValue) {
        var events = Signal.constant(defValue);
        function updateDropdown(node, oldModel, newModel) {
            node.currentModel = newModel;
            if (oldModel.options == newModel.options)
                return;
            var m;
            for (var n = node.firstChild; n != null; n = m) {
                var m = n.nextSibling;
                node.removeChild(n);
            }
            
            for (var i = 0; i < newModel.options.length; ++i) {
                var option = newNode('option');
                var name = JS.fromString(newModel.options[i]._0);
                option.value = name;
                option.innerHTML = name;
                node.appendChild(option);
            }
        }

        function renderDropdown(model) {
            var select = newNode('select');
            select.currentModel = model;
            select.addEventListener('change', function() {
                elm.notify(events.id, select.currentModel.options[select.selectedIndex]._1);
            });

            updateDropdown(select, {options: null}, model);
            return select;
        }

        var element = A3(newElement, 200, 25, {
            ctor: 'Custom',
            type: 'DynamicDropDown',
            render: renderDropdown,
            update: updateDropdown,
            model: { options: [{_0: defText, _1: defValue }] }
        });
        return Tuple2({ctor: 'DynamicDropdown', element: element }, events);
    }

    function dropDownToElement(dropDown, options) {
        var e = dropDown.element;
        var p = dropDown.element.props;
        dropDown.element =
            { props:
              { id: Utils.guid(), width: p.width,
                height: p.height, opacity: p.opacity,
                color: p.color, href: p.href, tag: p.tag,
                hover: p.hover },
              element: { ctor: 'Custom', type: 'DynamicDropdown',
                         render: e.element.render,
                         update: e.element.update,
                         model: { options: JS.fromList(options) }
                       }
            };
        return dropDown.element;
    }

    return elm.Native.Graphics.DynamicInput = { dynamicDropDown: F2(dynamicDropDown),
                                                dropDownToElement: F2(dropDownToElement) };
};
