import Graphics.UI.GLUT
import qualified Graphics.Rendering.OpenGL as GL
-- import LSystem For future use

--stack ghc -- --make main.hs
--credit LSystem.hs

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (0.1*k , 0.1*k , 0) | k <- [0..10] ]

data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

theCanvas :: Pnt
theCanvas  =  Pnt 800 800

pointToSize :: Pnt -> Size
pointToSize (Pnt x y) = Size (ceiling x) (ceiling y)

background :: IO ()
background = do GL.color theBGcolor
                GL.renderPrimitive GL.Polygon $ mapM_ GL.vertex
                      [GL.Vertex3 (-1) (-1) 0,
                       GL.Vertex3   1  (-1) 0,
                       GL.Vertex3   1    1  0,
                       GL.Vertex3 (-1)   1 (0::GL.GLfloat) ]

theBGcolor :: GL.Color3 GL.GLfloat
theBGcolor = GL.Color3 1.0 1.0 1.0

-- vertex3f :: (Float, Float, Float) -> (GLfloat,GLfloat,GLfloat)
-- vertex3f (x,y,z) = vertex $ Vertex3 x y (z :: GLfloat)

main :: IO ()
main = do
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize  $= pointToSize theCanvas 
    getArgsAndInitialize
    w <- createWindow "Turtle Graphics"
    displayCallback $= display
    reshapeCallback $= Just (\x -> (viewport $= (Position 0 0, x)))
    mainLoop
 
display :: DisplayCallback
display = do 
    clear [ColorBuffer]
    loadIdentity
    background -- for futuer use
    renderPrimitive Points $
        mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
    swapBuffers

                                