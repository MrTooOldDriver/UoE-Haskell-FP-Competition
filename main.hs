import Graphics.UI.GLUT
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad
import Function
import ColourProfile
import LinesSet

data Colour = Colour Float Float Float
    deriving (Eq,Ord,Show)

data ColourPointsGLfloat = ColourPointsGLfloat Colour Colour Colour Colour
    deriving (Eq,Ord,Show)

-- import Function
-- import LSystem For future use

--stack ghc -- --make main.hs
--credit LSystem.hs
data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)
-- data Colour = Colour Float Float Float
--  deriving (Eq,Ord,Show)

colourProfile = redToYellow

x :: Pnt -> Float
y :: Pnt -> Float
x (Pnt a _) = a
y (Pnt _ b) = b

-- r :: Colour -> Float
-- g :: Colour -> Float
-- b :: Colour -> Float
-- r (Colour a _ _) = a
-- g (Colour _ b _) = b
-- b (Colour _ _ c) = c


-- colourListGen :: ColourPointsGLfloat -> [(GLfloat,GLfloat,GLfloat)]
-- colourListGen (ColourPointsGLfloat c1 c2 c3 c4) =       
--         [(r c1 + (co*cdiffR c1 c2),g c1 + (co*cdiffG c1 c2),b c1 + (co*cdiffB c1 c2)) | co <- [0..3333]]
--         ++
--         [(r c2 + (co*cdiffR c2 c3),g c2 + (co*cdiffG c2 c3),b c2 + (co*cdiffB c2 c3)) | co <- [0..3333]]
--         ++
--         [(r c3 + (co*cdiffR c3 c4),g c3 + (co*cdiffG c3 c4),b c3 + (co*cdiffB c3 c4)) | co <- [0..3333]]
--         where 
--                 cdiffR x y = (r y - r x) /3333
--                 cdiffG x y = (g y - g x) /3333
--                 cdiffB x y = (b y - b x) /3333


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


-- myPoints :: [(GLfloat,GLfloat,GLfloat)]
-- myPoints = [ (0.1*k , 0.1*k , 0) | k <- [0..10] ]

-- myBezier :: [(GLfloat,GLfloat,GLfloat)]
-- myBezier = [pntToPoint (test t)| t <- map (*0.0001) [0..10000] ]
--         where test = pointBezier (Pnt (-1.0) 0.0) (Pnt 0.0 0.3) (Pnt 0.0 (-0.3)) (Pnt 1.0 0.0)

-- myBezier1 :: [(GLfloat,GLfloat,GLfloat)]
-- myBezier1 = [pntToPoint (test t)| t <- map (*0.0001) [0..10000] ]
--         where test = pointBezier (Pnt (-1.0) 0.0) (Pnt 0.0 0.2) (Pnt 0.0 (-0.2)) (Pnt 1.0 0.0)

-- myBezier2 :: [(GLfloat,GLfloat,GLfloat)]
-- myBezier2 = [pntToPoint (test t)| t <- map (*0.0001) [0..10000] ]
--         where test = pointBezier (Pnt (-1.0) 0.0) (Pnt 0.0 0.1) (Pnt 0.0 (-0.1)) (Pnt 1.0 0.0)

--No colour bezier function--
bezier :: (Pnt,Pnt,Pnt,Pnt) -> [(GLfloat,GLfloat,GLfloat)]
bezier (a,b,c,d) = [pntToPoint (pointBezier a b c d t)| t <- map (*0.0001) [0..10000] ]
--end of No colour function--

--Colour control points
-- Warning 
-- tc1_RGB :: Colour
-- tc2_RGB :: Colour
-- tc3_RGB :: Colour
-- tc4_RGB :: Colour

--Red to Yellow
-- tc1_RGB = Colour 255 255 255
-- tc2_RGB = Colour 204 0 0
-- tc3_RGB = Colour 255 255 0
-- tc4_RGB = Colour 255 255 255

--Yellow to Green
-- tc1_RGB = Colour 255 255 255
-- tc2_RGB = Colour 255 255 0
-- tc3_RGB = Colour 102 255 102
-- tc4_RGB = Colour 255 255 255

--Green to blue
-- tc1_RGB = Colour 255 255 255
-- tc2_RGB = Colour 102 255 102
-- tc3_RGB = Colour 51 153 255
-- tc4_RGB = Colour 255 255 255

--Blue to Purple
-- tc1_RGB = Colour 255 255 255
-- tc2_RGB = Colour 51 153 255
-- tc3_RGB = Colour 153 51 255
-- tc4_RGB = Colour 255 255 255

--Purple to pink
-- tc1_RGB = Colour 255 255 255
-- tc2_RGB = Colour 153 51 255
-- tc3_RGB = Colour 255 0 127
-- tc4_RGB = Colour 255 255 255

--RainBow
-- tc1_RGB = Colour 204 0 0
-- tc2_RGB = Colour 255 255 0
-- tc3_RGB = Colour 51 255 51
-- tc4_RGB = Colour 51 51 255

-- tc1 :: Colour
-- tc2 :: Colour
-- tc3 :: Colour
-- tc4 :: Colour
-- tc1 = rgbToGLrgb tc1_RGB
-- tc2 = rgbToGLrgb tc2_RGB
-- tc3 = rgbToGLrgb tc3_RGB
-- tc4 = rgbToGLrgb tc4_RGB

-- rgbToGLrgb :: ColourPointsRGB -> Colour
-- rgbToGLrgb (ColourPointsRGB (Colour r) (Colour g) (Colour b)) = Colour (r/255) (g/255) (b/255)




--new colour bezier function--
bezierColour :: (Pnt,Pnt,Pnt,Pnt) -> [((GLfloat,GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat))]
bezierColour (a,b,c,d) = zip (colourListGen(rgbToGLrgb colourProfile)) [pntToPoint (pointBezier a b c d t) | t <- map (*0.0001) [0..10000] ]
--End--

-- pointsList :: [((Float,Float),(Float,Float),(Float,Float),(Float,Float))]
-- pointsList = [] 
--                 ++
--              [  ( ( -1.0, 0.365 ) ,  ( -0.1, 1.0 ) ,  ( 0.2, -0.60 ) , ( 1.0, 0.010 ) ),
--                 ( ( -1.0, 0.355 ) ,  ( -0.1, 1.0 ) ,  ( 0.2, -0.61 ),  ( 1.0, 0.009 ) ),
--                 ( ( -1.0, 0.345 ) ,  ( -0.1, 1.0 ) ,  ( 0.2, -0.62 ) , ( 1.0, 0.008 ) ),
--                 ( ( -1.0, 0.335 ) ,  ( -0.1, 1.0 ) ,  ( 0.2, -0.63 ) , ( 1.0, 0.007 ) ),
--                 ( ( -1.0, 0.325 ) ,  ( -0.1, 1.0 ) ,  ( 0.2, -0.64 ) , ( 1.0, 0.006 ) )]
--                 ++
--              [  ( ( -1.0, 0.300 ) ,  ( -0.1, 0.975 ) ,  ( 0.2, -0.65 ) , ( 1.0, 0.005 ) ),
--                 ( ( -1.0, 0.275 ) ,  ( -0.1, 0.950 ) ,  ( 0.2, -0.66 ),  ( 1.0, 0.004 ) ),
--                 ( ( -1.0, 0.250 ) ,  ( -0.1, 0.925 ) ,  ( 0.2, -0.67 ) , ( 1.0, 0.003 ) ),
--                 ( ( -1.0, 0.225 ) ,  ( -0.1, 0.900 ) ,  ( 0.2, -0.68 ) , ( 1.0, 0.002 ) ),
--                 ( ( -1.0, 0.200 ) ,  ( -0.1, 0.875 ) ,  ( 0.2, -0.69 ) , ( 1.0, 0.001 ) )]
--                 ++
--              [  ( ( -1.0, 0.175 ) ,  ( -0.1, 0.850 ) ,  ( 0.2, -0.70 ) , ( 1.0, -0.001 ) ),
--                 ( ( -1.0, 0.150 ) ,  ( -0.1, 0.825 ) ,  ( 0.2, -0.71 ),  ( 1.0, -0.002 ) ),
--                 ( ( -1.0, 0.125 ) ,  ( -0.1, 0.800 ) ,  ( 0.2, -0.72 ) , ( 1.0, -0.003 ) ),
--                 ( ( -1.0, 0.100 ) ,  ( -0.1, 0.775 ) ,  ( 0.2, -0.73 ) , ( 1.0, -0.004 ) ),
--                 ( ( -1.0, 0.075 ) ,  ( -0.1, 0.750 ) ,  ( 0.2, -0.74 ) , ( 1.0, -0.005 ) )]       
--                 ++
--              [  ( ( -1.0, 0.05 ) ,  ( -0.1, 0.725 ) ,  ( 0.2, -0.75 ) , ( 1.0, -0.006 ) ),
--                 ( ( -1.0, 0.04  ) ,  ( -0.1, 0.725 ) ,  ( 0.2, -0.76 ),  ( 1.0, -0.007 ) ),
--                 ( ( -1.0, 0.03  ) ,  ( -0.1, 0.725 ) ,  ( 0.2, -0.77 ) , ( 1.0, -0.008 ) ),
--                 ( ( -1.0, 0.02  ) ,  ( -0.1, 0.725 ) ,  ( 0.2, -0.78 ) , ( 1.0, -0.009 ) ),
--                 ( ( -1.0, 0.01  ) ,  ( -0.1, 0.725 ) ,  ( 0.2, -0.79 ) , ( 1.0, -0.010 ) )]

--                 ++                
--              [  ( ( -1.0, 0 ) ,  ( -0.4, -1.00) ,   ( 0.6, 0.7 ) , ( 1.0, 0.01 ) ),
--                 ( ( -1.0, 0 ) ,  ( -0.4, -0.99 ) ,  ( 0.6, 0.7 ) , ( 1.0, 0.02 ) ),
--                 ( ( -1.0, 0 ) ,  ( -0.4, -0.98 ) ,  ( 0.6, 0.7 ) , ( 1.0, 0.03 ) ),
--                 ( ( -1.0, 0 ) ,  ( -0.4, -0.97 ) ,  ( 0.6, 0.7 ) , ( 1.0, 0.04 ) ),
--                 ( ( -1.0, 0 ) ,  ( -0.4, -0.96 ) ,  ( 0.6, 0.7 ) , ( 1.0, 0.05 ) )]
--                 ++                
--              [  ( ( -1.0, 0 ) ,  ( -0.4, -0.950 ) ,  ( 0.6, 0.725 ) , ( 1.0, 0.075 ) ),
--                 ( ( -1.0, 0 ) ,  ( -0.4, -0.925 ) ,  ( 0.6, 0.750 ) , ( 1.0, 0.100 ) ),
--                 ( ( -1.0, 0 ) ,  ( -0.4, -0.900 ) ,  ( 0.6, 0.775 ) , ( 1.0, 0.125 ) ),
--                 ( ( -1.0, 0 ) ,  ( -0.4, -0.875 ) ,  ( 0.6, 0.800 ) , ( 1.0, 0.150 ) ),
--                 ( ( -1.0, 0 ) ,  ( -0.4, -0.850 ) ,  ( 0.6, 0.825 ) , ( 1.0, 0.175 ) )]
--                 ++                
--              [  ( ( -1.0, 0 ) ,  ( -0.4, -0.825 ) ,  ( 0.6, 0.850 ) , ( 1.0, 0.200 ) ),
--                 ( ( -1.0, 0 ) ,  ( -0.4, -0.800 ) ,  ( 0.6, 0.875 ) , ( 1.0, 0.225 ) ),
--                 ( ( -1.0, 0 ) ,  ( -0.4, -0.775 ) ,  ( 0.6, 0.900 ) , ( 1.0, 0.250 ) ),
--                 ( ( -1.0, 0 ) ,  ( -0.4, -0.750 ) ,  ( 0.6, 0.925 ) , ( 1.0, 0.275 ) ),
--                 ( ( -1.0, 0 ) ,  ( -0.4, -0.725 ) ,  ( 0.6, 0.950 ) , ( 1.0, 0.30 ) )]
--                 ++                
--              [  ( ( -1.0, 0 ) ,  ( -0.4, -0.715) ,   ( 0.6, 0.975 ) , ( 1.0, 0.31 ) ),
--                 ( ( -1.0, 0 ) ,  ( -0.4, -0.705 ) ,  ( 0.6, 0.975 ) , ( 1.0, 0.32 ) ),
--                 ( ( -1.0, 0 ) ,  ( -0.4, -0.695 ) ,  ( 0.6, 0.975 ) , ( 1.0, 0.33 ) ),
--                 ( ( -1.0, 0 ) ,  ( -0.4, -0.685 ) ,  ( 0.6, 0.975 ) , ( 1.0, 0.34 ) ),
--                 ( ( -1.0, 0 ) ,  ( -0.4, -0.675 ) ,  ( 0.6, 0.975 ) , ( 1.0, 0.35 ) )]                                



pointsListToPntList :: [((Float,Float),(Float,Float),(Float,Float),(Float,Float))] -> [(Pnt,Pnt,Pnt,Pnt)]
pointsListToPntList xs = [(toPnt a, toPnt b, toPnt c, toPnt d) |(a,b,c,d) <- xs]
        where toPnt (y,z) = Pnt y z

genDrawList :: [(Pnt,Pnt,Pnt,Pnt)] -> [(GLfloat,GLfloat,GLfloat)]
genDrawList = concatMap bezier

genDrawListColour :: [(Pnt,Pnt,Pnt,Pnt)] -> [((GLfloat,GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat))]
genDrawListColour = concatMap bezierColour



-- BezierLineGruop1 = genDrawList (pointsListToPntList pointsList)
--Helper block--

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

--End of helper block--

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

--Origin--
-- display :: DisplayCallback
-- display = do 
--         let color3f r g b = color $ Color3 r g (b :: GLfloat)
--             vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
--         clear [ColorBuffer]
--         loadIdentity
--         -- background -- for futuer use
--         renderPrimitive Points $
--                 mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (genDrawList (pointsListToPntList pointsList))
--         swapBuffers
--End of origin--

pSize :: GLfloat
pSize = 1.7

--testing block--
display :: DisplayCallback
display = do 
        clear [ColorBuffer]
        loadIdentity
        background -- for futuer use

        GL.pointSmooth $= GL.Enabled --smooth
        GL.pointSize $= pSize -- point size

        forM_ (genDrawListColour (pointsListToPntList pointsList)) $ \((r,g,b),(x,y,z)) ->
                preservingMatrix $ do
                        color $ Color3 r g b
                        renderPrimitive Points $
                                vertex $ Vertex3 x y z
        swapBuffers


                                