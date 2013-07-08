import Native.Graphics.WebGLScene as N
import Dict as D
import Mouse
import Keyboard
import Touch

data BasisFunction = CONSTANT | LINEAR_LAGRANGE | QUADRATIC_LAGRANGE |
                     CUBIC_LAGRANGE | LINEAR_SIMPLEX | QUADRATIC_SIMPLEX
type Element = Int
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

myPrimModel =
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
myPerspectiveMatrix = perspectiveMatrix 0.01 15 1 myFovRadians

data TranslationPlane = TranslateXY | TranslateXZ
data CameraModifyMode = CameraRotate | CameraTranslate TranslationPlane

type CameraMoveState = { cameraQuaternion: Quaternion, cameraTransformation: (Float, Float, Float),
                         processedPosition: (Int, Int),
                         mainTouch: Touch, 
                         wasDragging: Bool, 
                         cameraModifyMode: CameraModifyMode }

initialCameraMoveState : CameraMoveState
initialCameraMoveState = { cameraQuaternion = Quaternion (1,0,0,0),
                           cameraTransformation = (0, 0, 0-10),
                           processedPosition = (0,0), 
                           mainTouch = { x = 0, y = 0, id = 0, x0 = 0, y0 = 0, t0 = 0 },
                           wasDragging = False,
                           cameraModifyMode = CameraRotate }

keyboardAlt : [KeyCode] -> Bool
keyboardAlt keysDown = any (\x -> x == 18) keysDown 

cameraMoveState : Signal CameraMoveState
cameraMoveState = Signal.foldp updateCameraMoveState initialCameraMoveState (Signal.lift4 (,,,) Keyboard.shift Keyboard.ctrl Keyboard.keysDown Touch.touches)
updateCameraMoveState : (Bool, Bool, [KeyCode], [Touch]) -> CameraMoveState -> CameraMoveState
updateCameraMoveState (shift, ctrl, keysDown, touches) oldMoveState =
  let
    dragging = (length touches > 0)
  in
    if not dragging
      then {oldMoveState | wasDragging <- False }
      else
        let
          newCameraModifyMode = if (shift || length touches == 2)                       then CameraTranslate TranslateXY else
                                if (ctrl || keyboardAlt keysDown || length touches > 2) then CameraTranslate TranslateXZ 
                                else CameraRotate
        in
          if not oldMoveState.wasDragging
            then
              let pointer = head touches in
                { oldMoveState | wasDragging <- True, processedPosition <- (pointer.x, pointer.y), mainTouch <- head touches,
                                cameraModifyMode <- newCameraModifyMode}
            else
              let
                previousPointCatcher = filter (\t -> t.id == oldMoveState.mainTouch.id ) touches
                pointer = if length previousPointCatcher > 0 then head previousPointCatcher else head touches
                (lastX, lastY) = oldMoveState.processedPosition
              in
                case oldMoveState.cameraModifyMode of
                  CameraRotate ->
                    let
                      -- Moving the mouse all the way across rotates 1 radian.
                      
                      xProportional = clamp (0-1) 1 (2 * ((toFloat pointer.x) / (toFloat canvasWidth))  - 1)
                      yProportional = clamp (0-1) 1 (2 * ((toFloat pointer.y) / (toFloat canvasHeight)) - 1) 
                      radiusSquared = (xProportional * xProportional) + (yProportional * yProportional)
                      alpha = radiusSquared / 2.0

                      xChange = (toFloat (lastX - pointer.x)) / (toFloat canvasWidth)
                      yChange = (toFloat (lastY - pointer.y)) / (toFloat canvasHeight)

                      phi   = 2 * (1.0 - alpha) * xChange
                      theta = 2 * (1.0 - alpha ) * yChange
                      psi   = 2 * ((yProportional * xChange) - (xProportional * yChange))

                      rotQuaternion = eulerToQuaternion phi psi theta
                    in
                      { oldMoveState | cameraQuaternion <-
                        normaliseQuaternion ( oldMoveState.cameraQuaternion `multiplyQuaternion` rotQuaternion ),
                        processedPosition <- (pointer.x, pointer.y), cameraModifyMode <- newCameraModifyMode, mainTouch <- pointer }
                  CameraTranslate plane ->
                    let
                      distanceX = (toFloat (pointer.x - lastX)) / (toFloat canvasWidth)
                      distanceY = (toFloat (pointer.y - lastY)) / (toFloat canvasHeight)
                      translateBy = case plane of
                                      TranslateXY -> (distanceX, 0-distanceY, 0)
                                      TranslateXZ -> (distanceX, 0, distanceY)
                      (otx, oty, otz) = oldMoveState.cameraTransformation
                      (tx, ty, tz) =  translateBy
                    in
                      { oldMoveState |
                          cameraTransformation <- (otx + tx, oty + ty, otz + tz),
                          processedPosition <- (pointer.x, pointer.y), cameraModifyMode <- newCameraModifyMode, mainTouch <- pointer }

cameraMatrix : Signal Matrix4x4
cameraMatrix = Signal.lift (\x ->
  vectorToTransformMatrix x.cameraTransformation
     `multiply4x4`
  quaternionToRotationMatrix x.cameraQuaternion
  ) cameraMoveState

{-
-- There are 16 elements in the LV model; this identifies one.
data ElementID = ElementID Int
-- This identifies a parameter from the set of 40 parameters used for mu or lambda.
data GlobalMuLambda = GlobalMuLambda Int
-- This identifies a parameter from the set of 134 parameters used for theta.
data GlobalTheta = GlobalTheta Int
-- This identifies one of the 8 local nodes used for 
-}

canvasWidth : Int
canvasWidth = 500
canvasHeight : Int
canvasHeight = 500

initialDiffuseDirection = [0.3, 0.5, 0.8, 1]

main : Signal Element
main = 
  pureMain <~ cameraMatrix ~ Touch.touches

scene cameraMatrixValue = 
    let
      camPerspect = myPerspectiveMatrix `multiply4x4` cameraMatrixValue
      rotatedDiffuseDirection = tuple3FromList <| (mat4ToInverseMat3 cameraMatrixValue) `mat4x4xv` initialDiffuseDirection 
    in
      (
        (glSceneObject canvasWidth canvasHeight myPrimModel 
           (GLPrimSceneView { projection = transpose4x4 camPerspect,
                              ambientColour = GLColour 1 1 1,
                              diffuseColour = GLColour 1 0.5 0.5,
                              ambientIntensity = 0.4,
                              diffuseIntensity = 0.5,
                              diffuseDirection = rotatedDiffuseDirection })))

touchesDisplay touches =       
        (flow down . map asText) touches

pureMain cameraMatrix touches =
 (scene cameraMatrix)
 `above`
 (touchesDisplay touches)
