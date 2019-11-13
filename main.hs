import Graphics.UI.GLUT
import qualified Graphics.Rendering.OpenGL as GL
-- import Function
-- import LSystem For future use

--stack ghc -- --make main.hs
--credit LSystem.hs
data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

x :: Pnt -> Float
y :: Pnt -> Float
x (Pnt a _) = a
y (Pnt _ b) = b

-- 2 control points Bezier curve

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

-- test :: Float -> Pnt
-- test = pointBezier (Pnt 0.0 0.0) (Pnt 0.0 1.0) (Pnt 1.0 0.0) (Pnt 1.0 1.0)


myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (0.1*k , 0.1*k , 0) | k <- [0..10] ]

myBezier :: [(GLfloat,GLfloat,GLfloat)]
myBezier = [pntToPoint (test t)| t <- map (*0.0001) [0..10000] ]
        where test = pointBezier (Pnt (-1.0) 0.0) (Pnt 0.0 0.3) (Pnt 0.0 (-0.3)) (Pnt 1.0 0.0)

myBezier1 :: [(GLfloat,GLfloat,GLfloat)]
myBezier1 = [pntToPoint (test t)| t <- map (*0.0001) [0..10000] ]
        where test = pointBezier (Pnt (-1.0) 0.0) (Pnt 0.0 0.2) (Pnt 0.0 (-0.2)) (Pnt 1.0 0.0)

myBezier2 :: [(GLfloat,GLfloat,GLfloat)]
myBezier2 = [pntToPoint (test t)| t <- map (*0.0001) [0..10000] ]
        where test = pointBezier (Pnt (-1.0) 0.0) (Pnt 0.0 0.1) (Pnt 0.0 (-0.1)) (Pnt 1.0 0.0)

bezier :: (Pnt,Pnt,Pnt,Pnt) -> [(GLfloat,GLfloat,GLfloat)]
bezier (a,b,c,d) = [pntToPoint (pointBezier a b c d t)| t <- map (*0.0001) [0..10000] ]

pointsList :: [((Float,Float),(Float,Float),(Float,Float),(Float,Float))]
pointsList = [] 
        --         ++
        --      [  ( ( -1.0, 0.0 ) ,  ( -0.5, 1.0 ) ,  ( 0.2, -0.2 ) , ( 1.0, 0.0 ) ),
        --         ( ( -1.0, 0.0 ) ,  ( -0.4, 1.0 ) ,  ( 0.2, -0.2 ),  ( 1.0, 0.0 ) ),
        --         ( ( -1.0, 0.0 ) ,  ( -0.3, 1.0 ) ,  ( 0.2, -0.2 ) , ( 1.0, 0.0 ) ),
        --         ( ( -1.0, 0.0 ) ,  ( -0.2, 1.0 ) ,  ( 0.2, -0.2 ) , ( 1.0, 0.0 ) ),
        --         ( ( -1.0, 0.0 ) ,  ( -0.1, 1.0 ) ,  ( 0.2, -0.2 ) , ( 1.0, 0.0 ) )]
        --         ++
        --      [  ( ( -1.0, 0.0 ) ,  ( -0.5, 1.0 ) ,  ( 0.2, -0.2 ) , ( 1.0, 0.0 ) ),
        --         ( ( -1.0, 0.0 ) ,  ( -0.5, 1.0 ) ,  ( 0.2, -0.3 ),  ( 1.0, 0.0 ) ),
        --         ( ( -1.0, 0.0 ) ,  ( -0.5, 1.0 ) ,  ( 0.2, -0.4 ) , ( 1.0, 0.0 ) ),
        --         ( ( -1.0, 0.0 ) ,  ( -0.5, 1.0 ) ,  ( 0.2, -0.5 ) , ( 1.0, 0.0 ) ),
        --         ( ( -1.0, 0.0 ) ,  ( -0.5, 1.0 ) ,  ( 0.2, -0.6 ) , ( 1.0, 0.0 ) ) ]
                ++
             [  ( ( -1.0, -0.01 ) ,  ( -0.1, 1.0 ) ,  ( 0.2, -0.20 ) , ( 1.0, 0.0 ) ),
                ( ( -1.0, -0.02 ) ,  ( -0.1, 1.0 ) ,  ( 0.2, -0.21 ),  ( 1.0, 0.0 ) ),
                ( ( -1.0, -0.03 ) ,  ( -0.1, 1.0 ) ,  ( 0.2, -0.22 ) , ( 1.0, 0.0 ) ),
                ( ( -1.0, -0.04 ) ,  ( -0.1, 1.0 ) ,  ( 0.2, -0.23 ) , ( 1.0, 0.0 ) ),
                ( ( -1.0, -0.05 ) ,  ( -0.1, 1.0 ) ,  ( 0.2, -0.24 ) , ( 1.0, 0.0 ) )]
                ++
             [  ( ( -1.0, -0.075 ) ,  ( -0.1, 0.975 ) ,  ( 0.2, -0.25 ) , ( 1.0, 0.0 ) ),
                ( ( -1.0, -0.100 ) ,  ( -0.1, 0.950 ) ,  ( 0.2, -0.26 ),  ( 1.0, 0.0 ) ),
                ( ( -1.0, -0.125 ) ,  ( -0.1, 0.925 ) ,  ( 0.2, -0.27 ) , ( 1.0, 0.0 ) ),
                ( ( -1.0, -0.150 ) ,  ( -0.1, 0.900 ) ,  ( 0.2, -0.28 ) , ( 1.0, 0.0 ) ),
                ( ( -1.0, -0.175 ) ,  ( -0.1, 0.875 ) ,  ( 0.2, -0.29 ) , ( 1.0, 0.0 ) )]
                ++
             [  ( ( -1.0, -0.200 ) ,  ( -0.1, 0.850 ) ,  ( 0.2, -0.30 ) , ( 1.0, 0.0 ) ),
                ( ( -1.0, -0.225 ) ,  ( -0.1, 0.825 ) ,  ( 0.2, -0.31 ),  ( 1.0, 0.0 ) ),
                ( ( -1.0, -0.250 ) ,  ( -0.1, 0.800 ) ,  ( 0.2, -0.32 ) , ( 1.0, 0.0 ) ),
                ( ( -1.0, -0.275 ) ,  ( -0.1, 0.775 ) ,  ( 0.2, -0.33 ) , ( 1.0, 0.0 ) ),
                ( ( -1.0, -0.300 ) ,  ( -0.1, 0.750 ) ,  ( 0.2, -0.34 ) , ( 1.0, 0.0 ) )]       
                ++
             [  ( ( -1.0, -0.325 ) ,  ( -0.1, 0.725 ) ,  ( 0.2, -0.35 ) , ( 1.0, 0.0 ) ),
                ( ( -1.0, -0.335 ) ,  ( -0.1, 0.725 ) ,  ( 0.2, -0.36 ),  ( 1.0, 0.0 ) ),
                ( ( -1.0, -0.345 ) ,  ( -0.1, 0.725 ) ,  ( 0.2, -0.37 ) , ( 1.0, 0.0 ) ),
                ( ( -1.0, -0.355 ) ,  ( -0.1, 0.725 ) ,  ( 0.2, -0.38 ) , ( 1.0, 0.0 ) ),
                ( ( -1.0, -0.365 ) ,  ( -0.1, 0.725 ) ,  ( 0.2, -0.39 ) , ( 1.0, 0.0 ) )]                


                -- ++
        --      [  ( ( -1.0, -0.025 ) ,  ( -0.1, 1.0 ) ,  ( 0.2, -0.20 ) , ( 1.0, 0.0 ) ),
        --         ( ( -1.0, -0.050 ) ,  ( -0.1, 1.0 ) ,  ( 0.2, -0.21 ),  ( 1.0, 0.0 ) ),
        --         ( ( -1.0, -0.075 ) ,  ( -0.1, 1.0 ) ,  ( 0.2, -0.22 ) , ( 1.0, 0.0 ) ),
        --         ( ( -1.0, -0.100 ) ,  ( -0.1, 1.0 ) ,  ( 0.2, -0.23 ) , ( 1.0, 0.0 ) ),
        --         ( ( -1.0, -0.125 ) ,  ( -0.1, 1.0 ) ,  ( 0.2, -0.24 ) , ( 1.0, 0.0 ) )]


pointsListToPntList :: [((Float,Float),(Float,Float),(Float,Float),(Float,Float))] -> [(Pnt,Pnt,Pnt,Pnt)]
pointsListToPntList xs = [(toPnt a, toPnt b, toPnt c, toPnt d) |(a,b,c,d) <- xs]
        where toPnt (y,z) = Pnt y z

genDrawList :: [(Pnt,Pnt,Pnt,Pnt)] -> [(GLfloat,GLfloat,GLfloat)]
genDrawList = concatMap bezier

-- BezierLineGruop1 = genDrawList (pointsListToPntList pointsList)


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
    -- background -- for futuer use
    renderPrimitive Points $
        mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (genDrawList (pointsListToPntList pointsList))
    swapBuffers

                                