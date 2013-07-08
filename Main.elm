import Native.Graphics.WebGLScene as N
import Dict
import Mouse
import Keyboard
import Signal
import Http (Success, Waiting, Failure)
import Json
import JavaScript.Experimental

data BasisFunction = CONSTANT | LINEAR_LAGRANGE | QUADRATIC_LAGRANGE |
                     CUBIC_LAGRANGE | LINEAR_SIMPLEX | QUADRATIC_SIMPLEX
type Element = Int
-- 3D cartesian coordinates.
data Coord3D = Coord3D Float Float Float
data Triangle = Triangle Coord3D Coord3D Coord3D
data Shape = LINE | SQUARE | TRIANGLE | CUBE | TETRAHEDRON | WEDGE12 | WEDGE13 | WEDGE23
type GLProjection = Matrix4x4
data GLColour = GLColour Float Float Float
data Matrix4x4 = Matrix4x4 [Float]
data Quaternion = Quaternion (Float, Float, Float, Float)

data GLPrimModel = GLPrimModel {
  -- A list of primitive triangles.
  primitives: [Triangle]
  }

data GLPrimSceneView = GLPrimSceneView {
  projection: GLProjection, -- A projection / camera matrix to apply.
  ambientColour: GLColour,  -- The ambient colour.
  diffuseColour: GLColour,  -- The diffuse light colour.
  ambientIntensity: Float,  -- The intensity in [0,1] of the ambient light.
  diffuseIntensity: Float,  -- The intensity in [0,1] of the diffuse light.
  diffuseDirection: (Float, Float, Float) -- The diffuse light direction vector.
  }

-- Constructs a WebGL element to view a model according to settings.
glSceneObject : Int -> Int -> GLPrimModel -> GLPrimSceneView -> Element
glSceneObject = N.glSceneObject

-- Some basic matrix maths specific to 4x4 matrices...
identity4x4 = Matrix4x4 [1,0,0,0,
                         0,1,0,0,
                         0,0,1,0,
                         0,0,0,1]

tuple3FromList : [Float] -> (Float, Float, Float)
tuple3FromList [x, y, z, w] = (x, y, z)

matrix4x4Row : Int -> Matrix4x4 -> [Float]
matrix4x4Row row (Matrix4x4 [x11,x12,x13,x14,
                             x21,x22,x23,x24,
                             x31,x32,x33,x34,
                             x41,x42,x43,x44]) =
  case row of
    0 -> [x11, x12, x13, x14]
    1 -> [x21, x22, x23, x24]
    2 -> [x31, x32, x33, x34]
    3 -> [x41, x42, x43, x44]

matrix4x4Column : Int -> Matrix4x4 -> [Float]
matrix4x4Column row (Matrix4x4 [x11,x12,x13,x14,
                                x21,x22,x23,x24,
                                x31,x32,x33,x34,
                                x41,x42,x43,x44]) =
  case row of
    0 -> [x11, x21, x31, x41]
    1 -> [x12, x22, x32, x42]
    2 -> [x13, x23, x33, x43]
    3 -> [x14, x24, x34, x44]

dotProduct : [Float] -> [Float] -> Float
dotProduct a b = sum (zipWith (*) a b)

multiply4x4 : Matrix4x4 -> Matrix4x4 -> Matrix4x4
multiply4x4 m1 m2 =
  let
    p r c = dotProduct (matrix4x4Row r m1) (matrix4x4Column c m2)
  in
   Matrix4x4 [p 0 0, p 0 1, p 0 2, p 0 3,
              p 1 0, p 1 1, p 1 2, p 1 3,
              p 2 0, p 2 1, p 2 2, p 2 3,
              p 3 0, p 3 1, p 3 2, p 3 3
             ]
mat4x4xv : Matrix4x4 -> [Float] -> [Float]
mat4x4xv m v =
  [dotProduct (matrix4x4Row 0 m) v,
   dotProduct (matrix4x4Row 1 m) v,
   dotProduct (matrix4x4Row 2 m) v,
   dotProduct (matrix4x4Row 3 m) v]

transpose4x4 : Matrix4x4 -> Matrix4x4
transpose4x4 (Matrix4x4 [x11,x12,x13,x14,
                         x21,x22,x23,x24,
                         x31,x32,x33,x34,
                         x41,x42,x43,x44]) =
  Matrix4x4 [x11, x21, x31, x41,
             x12, x22, x32, x42,
             x13, x23, x33, x43,
             x14, x24, x34, x44]

norm4 : [Float] -> [Float]
norm4 [x1, x2, x3, x4] = [x1/x4, x2/x4, x3/x4]


-- | mat4ToInverseMat3 calculates the inverse of the upper 3x3 elements of a 4x4 matrix, returning a 4x4 matrix containing the result in its upper 3x3, and the identiy in the lower right.
--   Based on code from glMatrix.
mat4ToInverseMat3 : Matrix4x4 -> Matrix4x4
mat4ToInverseMat3 (Matrix4x4 
  [x11,x12,x13,x14,
   x21,x22,x23,x24,
   x31,x32,x33,x34,
   x41,x42,x43,x44]) =
  let
    b12 = x33*x22-x23*x32
    b22 = x23*x31-x33*x21
    b32 = x32*x21-x22*x31
         
    det = x11*b12 + x12*b22 + x13*b32
    invDet = if det == 0 then 0 else 1/det
  in
    Matrix4x4 [
      b12*invDet,  (x13*x32 - x33*x12)*invDet,  (x23*x12 - x13*x22)*invDet, 0,
      b22*invDet,  (x33*x11 - x13*x31)*invDet,  (x13*x21 - x23*x11)*invDet, 0,
      b32*invDet,  (x12*x31 - x32*x11)*invDet,  (x22*x11 - x12*x21)*invDet, 0,
      0,           0,                           0,                          1
      ]

quaternionToRotationMatrix (Quaternion (a, b, c, d)) =
  Matrix4x4 [1 - 2 * (c * c + d * d), 2 * (b * c + a * d),     2 * (b * d - a * c),     0,
             2 * (b * c - a * d),     1 - 2 * (b * b + d * d), 2 * (c * d + a * b),     0,
             2 * (b * d + a * c),     2 * (c * d - a * b),     1 - 2 * (b * b + c * c), 0,
             0,                       0,                       0,                       1]

normaliseQuaternion (Quaternion (a, b, c, d)) =
  let
    norm = sqrt (a * a + b * b + c * c + d * d)
  in
   Quaternion (a/norm, b/norm, c/norm, d/norm)
  
multiplyQuaternion (Quaternion (a1, b1, c1, d1)) (Quaternion (a2, b2, c2, d2)) =
  Quaternion (a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2,
              c1 * d2 - d1 * c2 + a1 * b2 + a2 * b1,
              d1 * b2 - b1 * d2 + a1 * c2 + a2 * c1,
              b1 * c2 - c1 * b2 + a1 * d2 + d1 * a2)
conjugateQuaternion (Quaternion (a, b, c, d)) = Quaternion (a, 0-b, 0-c, 0-d)

rotateVectorByQuaternion : Quaternion -> (Float, Float, Float) -> (Float, Float, Float)
rotateVectorByQuaternion q (x,y,z) =
  let
    Quaternion (_, x', y', z') =
      q `multiplyQuaternion` (Quaternion (0, x, y, z)) `multiplyQuaternion` (conjugateQuaternion q)
  in
   (x', y', z')

eulerToQuaternion theta phi psi =
  let
    theta2 = theta / 2
    phi2 = phi / 2
    psi2 = psi / 2
    costheta2 = cos theta2
    sintheta2 = sin theta2
    cosphi2 = cos phi2
    sinphi2 = sin phi2
    cospsi2 = cos psi2
    sinpsi2 = sin psi2
  in
   Quaternion (cospsi2 * costheta2 * cosphi2 + sinpsi2 * costheta2 * cosphi2,
               sinpsi2 * costheta2 * cosphi2 - cospsi2 * sintheta2 * sinphi2,
               cospsi2 * sintheta2 * cosphi2 + sinpsi2 * costheta2 * sinphi2,
               cospsi2 * costheta2 * sinphi2 - sinpsi2 * sintheta2 * cosphi2)

vectorToTransformMatrix (x, y, z) =
  Matrix4x4 [1, 0, 0, x,
             0, 1, 0, y,
             0, 0, 1, z,
             0, 0, 0, 1]

lowVal = 0-0.8
highVal = 0.8
lowZ = 0-0.8
highZ = 0.8

placeholderModel =
  let
    -- l = left, r = right, b = bottom, t = top, f = front, a = back.
    clbf = Coord3D lowVal lowVal lowZ
    crbf = Coord3D highVal lowVal lowZ
    cltf = Coord3D lowVal highVal lowZ
    crtf = Coord3D highVal highVal lowZ
    clba = Coord3D lowVal lowVal highZ
    crba = Coord3D highVal lowVal highZ
    clta = Coord3D lowVal highVal highZ
    crta = Coord3D highVal highVal highZ
    quad a b c d = [Triangle a b c, Triangle a c d]
  in
   GLPrimModel {
     -- Vertex ordering: anti-clockwise around external surface.
     primitives =
        quad clbf crbf crtf cltf ++ -- front
        quad clba clbf cltf clta ++ -- left
        quad crbf crba crta crtf ++ -- right
        quad clba clta crta crba ++ -- back
        quad cltf crtf crta clta ++ -- top
        quad crbf clbf clba crba ++ -- bottom
        []
               }

myFovRadians = pi / 4

perspectiveMatrix near far aspectRatio fovRadians =
  let
    f = 1.0 / (tan(fovRadians / 2))
    nf = 1 / (near - far)
  in
    transpose4x4 <| Matrix4x4 [
      f / aspectRatio,    0,     0,                      0,
      0,                  f,     0,                      0,
      0,                  0,     (far + near) * nf,      0 - 1,
      0,                  0,     (2 * far * near) * nf,  0
      ]

myPerspectiveMatrix : Matrix4x4
myPerspectiveMatrix = perspectiveMatrix 0.01 1000 1 myFovRadians

data TranslationPlane = TranslateXY | TranslateXZ
data CameraModifyMode = CameraRotate | CameraTranslate TranslationPlane

type CameraMoveState = { cameraQuaternion: Quaternion, cameraTransformation: (Float, Float, Float),
                         processedPosition: (Int, Int), mouseWasDown: Bool, 
                         cameraModifyMode: CameraModifyMode }
initialCameraMoveState : CameraMoveState
initialCameraMoveState = { cameraQuaternion = Quaternion (1,0,0,0),
                           cameraTransformation = (0, 0, 0-200),
                           processedPosition = (0,0), mouseWasDown = False, 
                           cameraModifyMode = CameraRotate }

keyboardAlt : [KeyCode] -> Bool
keyboardAlt keysDown = any (\x -> x == 18) keysDown 

cameraMoveState : Signal CameraMoveState
cameraMoveState = Signal.foldp updateCameraMoveState initialCameraMoveState (Signal.lift5 (,,,,) Mouse.isDown Keyboard.shift Keyboard.ctrl Keyboard.keysDown Mouse.position)
updateCameraMoveState : (Bool, Bool, Bool, [KeyCode], (Int, Int)) -> CameraMoveState -> CameraMoveState
updateCameraMoveState (mouseDown, shift, ctrl, keysDown, (mouseX, mouseY) as mousePos) oldMoveState =
  if not mouseDown
    then {oldMoveState | mouseWasDown <- False }
    else
      if not oldMoveState.mouseWasDown
        then { oldMoveState | mouseWasDown <- True, processedPosition <- mousePos,
                              cameraModifyMode <- if shift then CameraTranslate TranslateXY else
                                                     if (ctrl || keyboardAlt keysDown) then CameraTranslate TranslateXZ else CameraRotate}
        else
          case oldMoveState.cameraModifyMode of
            CameraRotate ->
              let
                (lastX, lastY) = oldMoveState.processedPosition
                -- Moving the mouse all the way across rotates 1 radian.
                phi   = (toFloat (lastX - mouseX)) / (toFloat canvasWidth)
                theta = (toFloat (lastY - mouseY)) / (toFloat canvasHeight)
                rotQuaternion = eulerToQuaternion phi 0 theta
              in
                { oldMoveState | cameraQuaternion <-
                  normaliseQuaternion ( oldMoveState.cameraQuaternion `multiplyQuaternion` rotQuaternion ),
                  processedPosition <- mousePos }
            CameraTranslate plane ->
              let
                (lastX, lastY) = oldMoveState.processedPosition
                distanceX = (toFloat (mouseX - lastX)) / (toFloat canvasWidth) * 50.0
                distanceY = (toFloat (mouseY - lastY)) / (toFloat canvasHeight) * 50.0
                translateBy = case plane of
                                TranslateXY -> (distanceX, 0-distanceY, 0)
                                TranslateXZ -> (distanceX, 0, distanceY)
                (otx, oty, otz) = oldMoveState.cameraTransformation
                (tx, ty, tz) =  translateBy
              in
                { oldMoveState |
                    cameraTransformation <- (otx + tx, oty + ty, otz + tz),
                    processedPosition <- mousePos }

cameraMatrix : Signal Matrix4x4
cameraMatrix = Signal.lift (\x ->
  vectorToTransformMatrix x.cameraTransformation
     `multiply4x4`
  quaternionToRotationMatrix x.cameraQuaternion
  ) cameraMoveState

-- This identifies a parameter from the set of 40 parameters used for mu or lambda.
data GlobalMuLambda = GlobalMuLambda Int
-- This identifies a parameter from the set of 134 parameters used for theta.
data GlobalTheta = GlobalTheta Int

-- There are 16 elements in the LV model; this identifies one (1..16).
data ElementID = ElementID Int
-- There are 16 local nodes in the bicubic interpolation (1..16)
data BicubicLocalNode = BicubicLocalNode Int
-- There are 4 local nodes in the bilinear interpolation (1..4)
data BilinearLocalNode = BilinearLocalNode Int
-- There are two surfaces - endo and epicardial.
data Surface = Endocardial | Epicardial

-- For graphical purposes, we refine each element into 3*3 quads, each containing
-- 4x4 nodes. This identifies one of those nodes within an element (as a number in
-- [1,16]).
data RefinedNodeID = RefinedNodeID Int

-- A prolate spheriodal coordinate, given a fixed focal length.
data Prolate = Prolate { lambda: Float, mu: Float, theta: Float }
data Xi = Xi (Float, Float)

canvasWidth : Int
canvasWidth = 500
canvasHeight : Int
canvasHeight = 500

initialDiffuseDirection = [0.3, 0.8, 0 - 0.5, 1]

baseModel = Signal.lift lvResponseToModel (Http.sendGet (Signal.constant "../LV.json"))

surfaceToInt x = case x of
  Endocardial -> 0
  Epicardial -> 1

nElements = 16
nBicubicLocalNodes = 16
nBilinearLocalNodes = 4

localNodeIndexBicubic : BicubicLocalNode -> ElementID -> Surface -> Int
localNodeIndexBicubic (BicubicLocalNode lnID) (ElementID elID) surf =
  (surfaceToInt surf) * nElements * nBicubicLocalNodes + (elID - 1) * nBicubicLocalNodes + (lnID - 1)

localNodeIndexBilinear : BilinearLocalNode -> ElementID -> Surface -> Int
localNodeIndexBilinear (BilinearLocalNode lnID) (ElementID elID) surf =
  (surfaceToInt surf) * nElements * nBilinearLocalNodes + (elID - 1) * nBicubicLocalNodes + (lnID - 1)

allRefinedNodes : [RefinedNodeID]
allRefinedNodes = map RefinedNodeID [1..16]

allElements : [ElementID]
allElements = map ElementID [1..nElements]

allSurfaces : [Surface]
allSurfaces = [Endocardial, Epicardial]

refinedNodeToXiCoordinates : RefinedNodeID -> Xi
refinedNodeToXiCoordinates (RefinedNodeID rnid) =
  let
    rnid0 = rnid - 1
    col = rnid0 `mod` 4
    row = rnid `div` 4
  in
   Xi ((toFloat col) * 0.333333333333333333333,
       (toFloat row) * 0.333333333333333333333)

cosh : Float -> Float
cosh x = (exp x + exp (0-x)) * 0.5
sinh : Float -> Float
sinh x = (exp x - exp (0-x)) * 0.5

-- Converts prolate spheroidal to cartesian
prolateToCartesian : Float -> Prolate -> Coord3D
prolateToCartesian focalLength (Prolate { lambda, mu, theta }) =
  Coord3D (focalLength * cosh lambda * cos mu) (focalLength * sinh lambda * sin mu * cos theta)
          (focalLength * sinh lambda * sin mu * sin theta)


lvResponseToModel : Response String -> GLPrimModel
lvResponseToModel resp =
  case resp of
    Waiting -> placeholderModel
    Failure _ _ -> placeholderModel
    Success raw ->
      case Json.fromString raw of
        Nothing -> placeholderModel
        Just m ->
          let
            rawData : { localToGlobalMuTheta: [{loc: Int, glob: Int}],
                        localToGlobalLambda: [{loc: Int, globs: [{glob: Int, mup: Float}]}],
                        distributions: [{dataset: String, coordScheme: String, analysis: String,
                                         biologicalState: String, components: Int,
                                         averages: { flength: Float, lambdas: [Float], mus: [Float],
                                                     thetas: [Float]}}] }
            rawData = JavaScript.Experimental.toRecord (Json.toJSObject m)
            bilinearLocalToGlobalMap = Dict.fromList . map (\x -> (x.loc, x.glob))  <| rawData.localToGlobalMuTheta
            bicubicLocalToGlobalsMap = Dict.fromList . map (\x -> (x.loc, x.globs))  <| rawData.localToGlobalLambda
            bilinearLookupValue values elid surf ln =
              Dict.findWithDefault 0 (Dict.findWithDefault 0 (localNodeIndexBilinear ln elid surf)
                                   bilinearLocalToGlobalMap) values
            bicubicLookupValue values elid surf ln =
              sum <|
                map (\gm -> gm.mup * Dict.findWithDefault 0 gm.glob values)
                    (Dict.findWithDefault [] (localNodeIndexBicubic ln elid surf) bicubicLocalToGlobalsMap)
            baseLambdaValues = Dict.fromList (zip [1..512] (head rawData.distributions).averages.lambdas)
            baseMuValues = Dict.fromList (zip [1..128] (head rawData.distributions).averages.mus)
            baseThetaValues = Dict.fromList (zip [1..128] (head rawData.distributions).averages.thetas)
            lookupLambdaValue : ElementID -> Surface -> BicubicLocalNode -> Float
            lookupLambdaValue = bicubicLookupValue baseLambdaValues
            lookupMuValue : ElementID -> Surface -> BilinearLocalNode -> Float
            lookupMuValue = bilinearLookupValue baseMuValues
            lookupThetaValue : ElementID -> Surface -> BilinearLocalNode -> Float
            lookupThetaValue = bilinearLookupValue baseThetaValues
            doBilinearInterpolation : (BilinearLocalNode -> Float) -> Xi -> Float
            doBilinearInterpolation f (Xi (xi0, xi1)) =
              let
                f00 = f (BilinearLocalNode 1)
                f10 = f (BilinearLocalNode 2)
                f01 = f (BilinearLocalNode 3)
                f11 = f (BilinearLocalNode 4)
              in
                f00 * (1 - xi0) * (1 - xi1) + f10 * xi0 * (1 - xi1) + f01 * (1 - xi0) * xi1 + f11 * xi0 * xi1

            prolateCoords : ElementID -> Surface -> RefinedNodeID -> Prolate
            prolateCoords elid surf rnid =
              let
                -- We take a shortcut here: instead of doing bicubic interpolation,
                -- we notice that all the points we want fall exactly on local nodes
                -- used by the interpolations, so we look up values instead of
                -- interpolating.
                bicubicNode = (\(RefinedNodeID i) -> BicubicLocalNode i) rnid
                xi = refinedNodeToXiCoordinates rnid
               in
                Prolate { lambda = lookupLambdaValue elid surf bicubicNode,
                          mu = doBilinearInterpolation (lookupMuValue elid surf) xi,
                          theta = doBilinearInterpolation (lookupThetaValue elid surf) xi }
            rcCoords : ElementID -> Surface -> RefinedNodeID -> Coord3D
            rcCoords elID surf rnID = prolateToCartesian (head rawData.distributions).averages.flength
                                                         (prolateCoords elID surf rnID)
            elementPrims elID surf =
              let 
                [n11,n12,n13,n14,
                 n21,n22,n23,n24,
                 n31,n32,n33,n34,
                 n41,n42,n43,n44] = map (rcCoords elID surf) allRefinedNodes
              in
                [Triangle n11 n21 n12, Triangle n12 n21 n22, Triangle n12 n22 n13, Triangle n13 n22 n23,
                 Triangle n13 n23 n14, Triangle n14 n23 n24,
                 Triangle n21 n31 n22, Triangle n22 n31 n32, Triangle n22 n32 n23, Triangle n23 n32 n33,
                 Triangle n23 n33 n24, Triangle n24 n33 n34,
                 Triangle n31 n41 n32, Triangle n32 n41 n42, Triangle n32 n42 n33, Triangle n33 n42 n43,
                 Triangle n33 n43 n34, Triangle n34 n43 n44]
          in
           GLPrimModel { primitives =
                           concatMap (\surf -> concatMap (\elID -> elementPrims elID surf) allElements)
                                     allSurfaces }

main : Signal Element
main = 
  flip (flip Signal.lift2 cameraMatrix) baseModel (\cameraMatrixValue baseModelValue ->
    let
      camPerspect = myPerspectiveMatrix `multiply4x4` cameraMatrixValue
      rotatedDiffuseDirection = tuple3FromList <| (mat4ToInverseMat3 cameraMatrixValue) `mat4x4xv` initialDiffuseDirection
    in
     -- plainText (show baseModelValue)
     (glSceneObject canvasWidth canvasHeight baseModelValue
       (GLPrimSceneView { projection = transpose4x4 camPerspect,
                          ambientColour = GLColour 1 1 1,
                          diffuseColour = GLColour 1 1 1,
                          ambientIntensity = 0.4,
                          diffuseIntensity = 0.3,
                          diffuseDirection = rotatedDiffuseDirection }))
                                                  )
