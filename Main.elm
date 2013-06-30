import Native.Graphics.WebGLMesh as N
import Dict as D

data BasisFunction = CONSTANT | LINEAR_LAGRANGE | QUADRATIC_LAGRANGE |
                     CUBIC_LAGRANGE | LINEAR_SIMPLEX | QUADRATIC_SIMPLEX
type Element = Int
data Coord3D = Coord3D Float Float Float
data Triangle = Triangle Element Element Element
data Shape = LINE | SQUARE | TRIANGLE | CUBE | TETRAHEDRON | WEDGE12 | WEDGE13 | WEDGE23
type GLProjection = Matrix4x4
data Matrix4x4 = Matrix4x4 [Float]

data GLMesh = GLMesh {
  -- A list of node positions.
  nodalPositions: [Coord3D],
  -- A list of node indices per element (first node is 1).
  elements: [Triangle]
  }

-- A Zinc object views a model according to settings.
glMeshObject : Int -> Int -> GLMesh -> GLProjection -> Element
glMeshObject = N.glMeshObject

identity4x4 = Matrix4x4 [1,0,0,0,
                         0,1,0,0,
                         0,0,1,0,
                         0,0,0,1]

-- lowVal = 0-0.49
-- highVal = 0-0.25
lowVal = 0-1.0
highVal = 1.0
myMesh = GLMesh {
  nodalPositions = [Coord3D lowVal lowVal 0, Coord3D highVal lowVal 0,
                    Coord3D lowVal highVal 0
                    -- Coord3D highVal highVal lowVal,
                    -- Coord3D lowVal lowVal highVal, Coord3D highVal lowVal highVal,
                    -- Coord3D lowVal highVal highVal, Coord3D highVal highVal highVal
                   ],
  elements = [Triangle 0 1 2, Triangle 1 2 3, Triangle 0 1 4, Triangle 1 4 5,
              Triangle 0 2 4, Triangle 2 4 6, Triangle 1 3 5, Triangle 3 5 7,
              Triangle 2 3 6, Triangle 3 6 7]
              }

main : Element
main = glMeshObject 500 500 myMesh identity4x4
