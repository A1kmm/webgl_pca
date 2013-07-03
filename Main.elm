import Native.Graphics.WebGLScene as N
import Dict as D
import Mouse
import Keyboard
import Signal

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

-- A Zinc object views a model according to settings.
glSceneObject : Int -> Int -> GLPrimModel -> GLPrimSceneView -> Element
glSceneObject = N.glSceneObject

-- Some basic matrix maths specific to 4x4 matrices...
identity4x4 = Matrix4x4 [1,0,0,0,
                         0,1,0,0,
                         0,0,1,0,
                         0,0,0,1]

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

inverseRotateVectorByQuaternion : Quaternion -> (Float, Float, Float) -> (Float, Float, Float)
inverseRotateVectorByQuaternion q (x,y,z) =
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

myCubeSizeHalf = 2

myPrimModel =
  let
    -- l = left, r = right, b = bottom, t = top, f = front, a = back.
    clbf = Coord3D (0 - myCubeSizeHalf) (0 - myCubeSizeHalf) (0 - myCubeSizeHalf)
    crbf = Coord3D      myCubeSizeHalf  (0 - myCubeSizeHalf) (0 - myCubeSizeHalf)
    cltf = Coord3D (0 - myCubeSizeHalf)      myCubeSizeHalf  (0 - myCubeSizeHalf)
    crtf = Coord3D      myCubeSizeHalf       myCubeSizeHalf  (0 - myCubeSizeHalf)
    clba = Coord3D (0 - myCubeSizeHalf) (0 - myCubeSizeHalf)      myCubeSizeHalf
    crba = Coord3D      myCubeSizeHalf  (0 - myCubeSizeHalf)      myCubeSizeHalf
    clta = Coord3D (0 - myCubeSizeHalf)      myCubeSizeHalf       myCubeSizeHalf
    crta = Coord3D      myCubeSizeHalf       myCubeSizeHalf       myCubeSizeHalf
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
        quad clbf crbf crba clba ++ -- bottom
        []
               }

myFovRadians = pi / 32

perspectiveMatrix near far aspectRatio fovRadians =
  let
    cotHalfFov = 1 / (tan (fovRadians / 2))
  in
   Matrix4x4 [near * cotHalfFov / aspectRatio, 0, 0, 0,
              0, near * cotHalfFov, 0, 0,
              0, 0, near/(far - near), far * near / (near - far),
              0, 0, 1.0, 0.0]


myPerspectiveMatrix : Matrix4x4
myPerspectiveMatrix = perspectiveMatrix 0.01 15 1 myFovRadians

data TransformPlane = TransformXY | TransformXZ
data CameraModifyMode = CameraRotate | CameraTransform TransformPlane

type CameraMoveState = { cameraQuaternion: Quaternion, cameraTransformation: (Float, Float, Float),
                         processedPosition: (Int, Int), mouseWasDown: Bool, 
                         cameraModifyMode: CameraModifyMode }
initialCameraMoveState : CameraMoveState
initialCameraMoveState = { cameraQuaternion = Quaternion (1,0,0,0),
                           cameraTransformation = (0, 0, 4),
                           processedPosition = (0,0), mouseWasDown = False, 
                           cameraModifyMode = CameraRotate }

cameraMoveState : Signal CameraMoveState
cameraMoveState = Signal.foldp updateCameraMoveState initialCameraMoveState (Signal.lift4 (,,,) Mouse.isDown Keyboard.shift Keyboard.ctrl Mouse.position)
updateCameraMoveState : (Bool, Bool, Bool, (Int, Int)) -> CameraMoveState -> CameraMoveState
updateCameraMoveState (mouseDown, shift, ctrl, (mouseX, mouseY) as mousePos) oldMoveState =
  if not mouseDown
    then {oldMoveState | mouseWasDown <- False }
    else
      if not oldMoveState.mouseWasDown
        then { oldMoveState | mouseWasDown <- True, processedPosition <- mousePos,
                              cameraModifyMode <- if shift then CameraTransform TransformXY else
                                                     if ctrl then CameraTransform TransformXZ else CameraRotate}
        else
          case oldMoveState.cameraModifyMode of
            CameraRotate ->
              let
                (lastX, lastY) = oldMoveState.processedPosition
                -- Moving the mouse all the way across rotates 1 radian.
                phi = (toFloat (mouseX - lastX)) / (toFloat canvasWidth)
                theta = (toFloat (mouseY - lastY)) / (toFloat canvasHeight)
                rotQuaternion = eulerToQuaternion phi 0 theta
              in
                { oldMoveState | cameraQuaternion <-
                  normaliseQuaternion ( oldMoveState.cameraQuaternion `multiplyQuaternion` rotQuaternion ),
                  processedPosition <- mousePos }
            CameraTransform plane ->
              let
                (lastX, lastY) = oldMoveState.processedPosition
                distanceX = (toFloat (mouseX - lastX)) / (toFloat canvasWidth)
                distanceY = (toFloat (mouseY - lastY)) / (toFloat canvasHeight)
                transformBy = case plane of
                                TransformXY -> (distanceX, 0-distanceY, 0)
                                TransformXZ -> (distanceX, 0, distanceY)
                (otx, oty, otz) = oldMoveState.cameraTransformation
                (tx, ty, tz) = inverseRotateVectorByQuaternion (oldMoveState.cameraQuaternion) transformBy
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

canvasWidth : Int
canvasWidth = 500
canvasHeight : Int
canvasHeight = 500

main : Signal Element
main = 
  flip Signal.lift cameraMatrix (\cameraMatrixValue ->
                                  let camPerspect = myPerspectiveMatrix `multiply4x4` cameraMatrixValue
                                  in
                        
                                   (glSceneObject canvasWidth canvasHeight myPrimModel 
                                      (GLPrimSceneView { projection = transpose4x4 camPerspect,
                                                         ambientColour = GLColour 1 1 1,
                                                         diffuseColour = GLColour 1 0.5 0.5,
                                                         ambientIntensity = 0.4,
                                                         diffuseIntensity = 0.5,
                                                         diffuseDirection = (0.3, 0.8, 0.5) })))
