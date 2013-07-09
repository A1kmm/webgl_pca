Elm.Native.Graphics.WebGLScene = function(elm) {
    "use strict";

    var Render = ElmRuntime.use(ElmRuntime.Render.Element);
    var JS = Elm.JavaScript(elm);
    var D = Elm.Dict(elm);

    elm.Native = elm.Native || {};
    elm.Native.Graphics = elm.Native.Graphics || {};
    if (elm.Native.Graphics.WebGLScene) return elm.Native.Graphics.WebGLScene;
    
    // Simple matrix projection...
    var vertexShader = "attribute mediump vec3 aPos;\
                        attribute mediump vec3 aNormal;\
                        varying mediump vec3 normal;\
                        uniform mediump mat4 aProjection;\
                        void main(void) {\
                          normal = normalize(aNormal);\
                          gl_Position = aProjection * vec4(aPos, 1.0); \
                        }";
    // For now, everything is green.
    var fragmentShader =
        "varying mediump vec3 normal;\
         uniform mediump vec3 aAmbientColour, aDiffuse1Colour;\
         uniform mediump vec3 aDiffuse1Direction;\
         uniform mediump float aAmbientIntensity, aDiffuse1Intensity;\
         void main(void) {\
           mediump vec3 rgb = aAmbientColour * aAmbientIntensity +\
                      aDiffuse1Colour * aDiffuse1Intensity * \
                      clamp(dot(normal, normalize(aDiffuse1Direction)), 0.0, 1.0);\
           gl_FragColor = vec4(rgb, 1.0);\
                         }"

    var newElement = Elm.Graphics.Element(elm).newElement;
    function renderGLScene(model) {
        var jscanvas = document.createElement("canvas");
        jscanvas.width = model.w;
        jscanvas.height = model.h;

        setTimeout(function() {
            newContext(jscanvas);
            updateGLScene(jscanvas, {w:0,h:0, model: null, projection: null }, model);
        }, 100);
        return jscanvas;
    }

    function newContext(jscanvas) {
        var ctx = jscanvas.getContext("webgl") || jscanvas.getContext("experimental-webgl");
        ctx.viewport(0, 0, 500, 500);
        var shaderProgram = ctx.createProgram();

        ctx.enable(ctx.DEPTH_TEST);

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
                          aNormal: ctx.getAttribLocation(shaderProgram, "aNormal"),
                          aProjection: ctx.getUniformLocation(shaderProgram, "aProjection"),
                          aAmbientColour: ctx.getUniformLocation(shaderProgram, "aAmbientColour"),
                          aAmbientIntensity: ctx.getUniformLocation(shaderProgram, "aAmbientIntensity"),
                          aDiffuse1Colour: ctx.getUniformLocation(shaderProgram, "aDiffuse1Colour"),
                          aDiffuse1Direction: ctx.getUniformLocation(shaderProgram, "aDiffuse1Direction"),
                          aDiffuse1Intensity: ctx.getUniformLocation(shaderProgram, "aDiffuse1Intensity")
                        };

        ctx.enableVertexAttribArray(shadeData.aPos);
        ctx.enableVertexAttribArray(shadeData.aNormal);

        jscanvas.shadeData = shadeData;
        jscanvas.ctx = ctx;
        return ctx;
    }
    
    function updateGLScene(jscanvas, curr, next) {
        jscanvas.width = next.w;
        jscanvas.height = next.h;
        if (jscanvas.ctx == null)
            return;
        jscanvas.ctx.viewport(0, 0, next.w,
                              next.h);

        var modelChanged = next.model != curr.model;
        var sceneChanged = next.sceneview != curr.sceneview;
        
        if (modelChanged || sceneChanged) {
            jscanvas.ctx.clearColor(0.0, 0.0, 0.0, 1.0);
            jscanvas.ctx.clearDepth(65535);
            jscanvas.ctx.depthFunc(jscanvas.ctx.LESS);
            jscanvas.ctx.clear(jscanvas.ctx.COLOR_BUFFER_BIT |
                               jscanvas.ctx.DEPTH_BUFFER_BIT);
        }

        var jsmodel = next.model._0;

        if (modelChanged) {
            var jsprims = JS.fromList(jsmodel.primitives);
            jscanvas.nNodes = jsprims.length * 3;
            var nodalPos = new Float32Array(3 * jscanvas.nNodes);
            var normals = new Float32Array(3 * jscanvas.nNodes);
            for (var primID in jsprims) {
                var thisPrim = jsprims[primID];
                nodalPos.set([thisPrim._0._0, thisPrim._0._1, thisPrim._0._2,
                              thisPrim._1._0, thisPrim._1._1, thisPrim._1._2,
                              thisPrim._2._0, thisPrim._2._1, thisPrim._2._2,
                             ], 9*primID);
                // Compute the normal vector between edges 01 and 02 (we
                // use an anticlockwise winding).
                var ux = thisPrim._1._0 - thisPrim._0._0,
                    uy = thisPrim._1._1 - thisPrim._0._1,
                    uz = thisPrim._1._2 - thisPrim._0._2,
                    vx = thisPrim._2._0 - thisPrim._0._0,
                    vy = thisPrim._2._1 - thisPrim._0._1,
                    vz = thisPrim._2._2 - thisPrim._0._2,
                    Nx = uy * vz - uz * vy,
                    Ny = uz * vx - ux * vz,
                    Nz = ux * vy - uy * vx;
                // Send the non-normalised normal vector to the shader.
                normals.set([Nx, Ny, Nz, Nx, Ny, Nz, Nx, Ny, Nz], 9 * primID);
            }

            jscanvas.ctx.bindBuffer(jscanvas.ctx.ARRAY_BUFFER, jscanvas.ctx.createBuffer());
            jscanvas.ctx.bufferData(jscanvas.ctx.ARRAY_BUFFER, nodalPos, jscanvas.ctx.STATIC_DRAW);
            jscanvas.ctx.vertexAttribPointer(jscanvas.shadeData.aPos, 3, jscanvas.ctx.FLOAT, false, 0, 0);

            jscanvas.ctx.bindBuffer(jscanvas.ctx.ARRAY_BUFFER, jscanvas.ctx.createBuffer());
            jscanvas.ctx.bufferData(jscanvas.ctx.ARRAY_BUFFER, normals, jscanvas.ctx.STATIC_DRAW);
            jscanvas.ctx.vertexAttribPointer(jscanvas.shadeData.aNormal, 3, jscanvas.ctx.FLOAT, false, 0, 0);
        }

        if (sceneChanged) {
            var sv = next.sceneview._0;
            jscanvas.ctx.uniformMatrix4fv(jscanvas.shadeData.aProjection, false,
                                          [sv.projection._0._0, sv.projection._1._0, sv.projection._2._0, sv.projection._3._0,
                                           sv.projection._0._1, sv.projection._1._1, sv.projection._2._1, sv.projection._3._1,
                                           sv.projection._0._2, sv.projection._1._2, sv.projection._2._2, sv.projection._3._2,
                                           sv.projection._0._3, sv.projection._1._3, sv.projection._2._3, sv.projection._3._3
                                          ]);
            jscanvas.ctx.uniform1f(jscanvas.shadeData.aAmbientIntensity, sv.ambientIntensity);
            jscanvas.ctx.uniform1f(jscanvas.shadeData.aDiffuse1Intensity, sv.diffuseIntensity);
            jscanvas.ctx.uniform3f(jscanvas.shadeData.aAmbientColour, sv.ambientColour._0,
                                   sv.ambientColour._1, sv.ambientColour._2);
            jscanvas.ctx.uniform3f(jscanvas.shadeData.aDiffuse1Colour, sv.diffuseColour._0,
                                   sv.diffuseColour._1, sv.diffuseColour._2);
            jscanvas.ctx.uniform3f(jscanvas.shadeData.aDiffuse1Direction, sv.diffuseDirection._0,
                                   sv.diffuseDirection._1, sv.diffuseDirection._2);
        }

        if (modelChanged || sceneChanged) {
            jscanvas.ctx.drawArrays(jscanvas.ctx.TRIANGLES, 0, jscanvas.nNodes);
            jscanvas.ctx.flush();
        }

        return false;
    }

    function glSceneObject(w, h, model, sceneview) {
        return A3(newElement, w, h, {
            ctor: 'Custom',
	    type: 'WebGL',
	    render: renderGLScene,
	    update: updateGLScene,
            props: {},
	    model: {w:w, h:h, model: model, sceneview: sceneview }
	});
    }
    return elm.Native.Graphics.WebGLScene = { glSceneObject: F4(glSceneObject) };
};
