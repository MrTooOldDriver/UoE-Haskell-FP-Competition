import Graphics.UI.GLUT

--stack ghc -- --make main.hs
--credit LSystem.hs

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (0.1*k , 0.1*k , 0) | k <- [0..10] ]

-- theBGcolor :: GL.Color3 GL.GLfloat
-- theBGcolor = penToRGB white

-- vertex3f :: (Float, Float, Float) -> (GLfloat,GLfloat,GLfloat)
-- vertex3f (x,y,z) = vertex $ Vertex3 x y (z :: GLfloat)

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop
 
display :: DisplayCallback
display = do 
clear [ColorBuffer]
renderPrimitive Points $
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
flush
                                