Elm.Native.Graphics.WebGLMesh = function(elm) {
    "use strict";

    var Render = ElmRuntime.use(ElmRuntime.Render.Element);
    var JS = Elm.JavaScript(elm);
    var D = Elm.Dict(elm);

    elm.Native = elm.Native || {};
    elm.Native.Graphics = elm.Native.Graphics || {};
    if (elm.Native.Graphics.WebGLMesh) return elm.Native.Graphics.WebGLMesh;
    
    // Simple matrix projection...
    var vertexShader = "attribute vec3 aPos;\
                        uniform mat4 aProjection;\
                        void main(void) {\
                          vec4 pos = vec4(aPos, 1.0);\
                          gl_Position = pos; \
                        }";
    // For now, everything is red.
    var fragmentShader = "void main(void) { gl_FragColor = vec4(0.0,1.0,0.0,1.0); }"

    var newElement = Elm.Graphics.Element(elm).newElement;
    function renderGLMesh(model) {
        var jscanvas = document.createElement("canvas");

        setTimeout(function() {
            var fakeOldModel = newContext(jscanvas);
            updateGLMesh(jscanvas, fakeOldModel, model);
        }, 100);
        return jscanvas;
    }

    function newContext(jscanvas) {
        var ctx = jscanvas.getContext("webgl") || jscanvas.getContext("experimental-webgl");
        ctx.viewport(0, 0, 500, 500);
        var shaderProgram = ctx.createProgram();

        ctx.disable(ctx.DEPTH_TEST);

        var vertShad = ctx.createShader(ctx.VERTEX_SHADER);
        ctx.shaderSource(vertShad, vertexShader);
        ctx.compileShader(vertShad);
        ctx.attachShader(shaderProgram, vertShad);

        var fragShad = ctx.createShader(ctx.FRAGMENT_SHADER);
        ctx.shaderSource(fragShad, fragmentShader);
        ctx.compileShader(fragShad);
        ctx.attachShader(shaderProgram, fragShad);

        ctx.linkProgram(shaderProgram);
        ctx.useProgram(shaderProgram);

        var shadeData = { aPos: ctx.getAttribLocation(shaderProgram, "aPos"),
                          aProjection: ctx.getUniformLocation(shaderProgram, "aProjection")
                        };

        ctx.enableVertexAttribArray(shadeData.aPos);

        return {w:0,h:0,mesh:D.empty, projection: null, ctx: ctx,
                                shadeData: shadeData };
    }
    
    function updateGLMesh(jscanvas, curr, next) {
        next.ctx = curr.ctx;
        next.shadeData = curr.shadeData;

        jscanvas.width = next.w;
        jscanvas.height = next.h;
        next.ctx.viewport(0, 0, next.w,
                          next.h);
        alert("w=" + next.ctx.drawingBufferWidth + ", h=" + next.ctx.drawingBufferHeight);


        var meshChanged = next.mesh != curr.mesh;
        var projChanged = next.projection != curr.projection;
        
        if (meshChanged || projChanged) {
            next.ctx.clearColor(1.0, 0.0, 0.0, 1.0);
            next.ctx.clearDepth(-10000.0);
            next.ctx.clear(next.ctx.COLOR_BUFFER_BIT |
                           next.ctx.DEPTH_BUFFER_BIT);
        }

        var jsmesh = next.mesh._0;

        if (meshChanged) {
            var b = next.ctx.createBuffer();
            next.ctx.bindBuffer(next.ctx.ARRAY_BUFFER, b);
            var jsnpos = JS.fromList(jsmesh.nodalPositions);
            next.shadeData.nNodes = jsnpos.length;
            var nodalPos = new Float32Array(3 * next.shadeData.nNodes);
            for (var nodeID in jsnpos) {
                var thisNodePos = jsnpos[nodeID];
                nodalPos.set([thisNodePos._0, thisNodePos._1, thisNodePos._2], 3*nodeID);
            }
            next.ctx.bufferData(next.ctx.ARRAY_BUFFER, nodalPos, next.ctx.STATIC_DRAW);
            next.ctx.vertexAttribPointer(next.shadeData.aPos, 3, next.ctx.FLOAT, false, 0, 0);


            /*
            next.ctx.bindBuffer(next.ctx.ELEMENT_ARRAY_BUFFER, next.ctx.createBuffer());
            var jselems = JS.fromList(jsmesh.elements);
            next.shadeData.nElems = jselems.length;
            var elemBuf = new ArrayBuffer(2 * 3 * jselems.length);
            var elemSet = new Int16Array(elemBuf);
            for (var elemID in jselems) {
                var thisTriangle = jselems[elemID];
                elemSet.set([thisTriangle._0, thisTriangle._1, thisTriangle._2], 3 * elemID);
            }
            next.ctx.bufferData(next.ctx.ELEMENT_ARRAY_BUFFER, elemBuf, next.ctx.STATIC_COPY);
            */
        }

        /*
        if (projChanged) {
            next.ctx.uniformMatrix4fv(next.shadeData.aProjection, false,
                                      new Float32Array(JS.fromList(next.projection._0)));
        }
        */

        if (meshChanged || projChanged) {
            next.ctx.drawArrays(next.ctx.TRIANGLES, 0, next.shadeData.nNodes);
            // next.ctx.drawElements(next.ctx.TRIANGLES, next.shadeData.nElems, next.ctx.UNSIGNED_SHORT, 0);
            next.ctx.flush();
        }

        return false;
    }

    function glMeshObject(w, h, mesh, projection) {
        return A3(newElement, w, h, {
            ctor: 'Custom',
	    type: 'Collage',
	    render: renderGLMesh,
	    update: updateGLMesh,
            props: {},
	    model: {w:w, h:h, mesh: mesh, projection: projection }
	});
    }
    return elm.Native.Graphics.WebGLMesh = { glMeshObject: F4(glMeshObject) };
};
