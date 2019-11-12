module Function (

   )
    where

import Graphics.UI.GLUT

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

