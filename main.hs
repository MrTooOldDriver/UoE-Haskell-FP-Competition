import Graphics.UI.GLUT
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad
import ColourProfile
import LinesSet

--stack ghc -- --make main.hs
--credit LSystem.hs

--Change colour profile , background colour and point size here--
theBGcolor :: GL.Color3 GL.GLfloat
pSize :: GLfloat

colourProfile = redToYellow
pSize = 1.7
theBGcolor = GL.Color3 1.0 1.0 1.0
--End of manual changing--


data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

x :: Pnt -> Float
y :: Pnt -> Float
x (Pnt a _) = a
y (Pnt _ b) = b

pointBezier :: Pnt -> Pnt -> Pnt -> Pnt -> Float -> Pnt
pointBezier p0 p1 p2 p3 t = Pnt (x0*sf0 + x1*sf1 + x2*sf2 + x3*sf3) (y0*sf0 + y1*sf1 + y2*sf2 + y3*sf3)
    where
        sf0 =             (1-t)^3
        sf1 = 3 * t     * (1-t)^2
        sf2 = 3 * (t^2) * (1-t)
        sf3 =     t^3
        x0 = x p0
        x1 = x p1
        x2 = x p2
        x3 = x p3
        y0 = y p0
        y1 = y p1
        y2 = y p2
        y3 = y p3

-- --No colour bezier function--
-- bezier :: (Pnt,Pnt,Pnt,Pnt) -> [(GLfloat,GLfloat,GLfloat)]
-- bezier (a,b,c,d) = [pntToPoint (pointBezier a b c d t)| t <- map (*0.0001) [0..10000] ]
-- --end of No colour function--

--new colour bezier function--
bezierColour :: (Pnt,Pnt,Pnt,Pnt) -> [((GLfloat,GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat))]
bezierColour (a,b,c,d) = zip (colourListGen(rgbToGLrgb colourProfile)) [pntToPoint (pointBezier a b c d t) | t <- map (*0.0001) [0..10000] ]
--End--

pointsListToPntList :: [((Float,Float),(Float,Float),(Float,Float),(Float,Float))] -> [(Pnt,Pnt,Pnt,Pnt)]
pointsListToPntList xs = [(toPnt a, toPnt b, toPnt c, toPnt d) |(a,b,c,d) <- xs]
        where toPnt (y,z) = Pnt y z

-- genDrawList :: [(Pnt,Pnt,Pnt,Pnt)] -> [(GLfloat,GLfloat,GLfloat)]
-- genDrawList = concatMap bezier

genDrawListColour :: [(Pnt,Pnt,Pnt,Pnt)] -> [((GLfloat,GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat))]
genDrawListColour = concatMap bezierColour

--Helper block From LSystem--

pntToPoint :: Pnt -> (GLfloat,GLfloat,GLfloat)
pntToPoint (Pnt a b) = (a,b,0)

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

--End of helper block--

main :: IO ()
main = do
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize  $= pointToSize theCanvas 
    getArgsAndInitialize
    w <- createWindow "Bezier Curves Colour Wave"
    displayCallback $= display
    reshapeCallback $= Just (\x -> (viewport $= (Position 0 0, x)))
    mainLoop

display :: DisplayCallback
display = do 
        clear [ColorBuffer]
        loadIdentity
        background 

        GL.pointSmooth $= GL.Enabled --smooth
        GL.pointSize $= pSize --Point size

        forM_ (genDrawListColour (pointsListToPntList pointsList)) $ \((r,g,b),(x,y,z)) ->
                preservingMatrix $ do
                        color $ Color3 r g b
                        renderPrimitive Points $
                                vertex $ Vertex3 x y z
        swapBuffers


                                