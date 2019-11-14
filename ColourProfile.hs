module ColourProfile
    (
        ColourPointsRGB,
        redToYellow,
        yellowToGreen,
        greenToBlue,
        blueToPurple,
        purpleToPink,
        rainbow,
        rgbToGLrgb,
        ColourPointsGLfloat,
        colourListGen
    )
    where
import Graphics.UI.GLUT

data Colour = Colour Float Float Float
    deriving (Eq,Ord,Show)

data ColourPointsRGB = ColourPointsRGB Colour Colour Colour Colour
    deriving (Eq,Ord,Show)

data ColourPointsGLfloat = ColourPointsGLfloat Colour Colour Colour Colour
    deriving (Eq,Ord,Show)

r :: Colour -> Float
g :: Colour -> Float
b :: Colour -> Float
r (Colour a _ _) = a
g (Colour _ b _) = b
b (Colour _ _ c) = c

redToYellow :: ColourPointsRGB
redToYellow = ColourPointsRGB tc1 tc2 tc3 tc4
        where
        tc1 = Colour 255 255 255
        tc2 = Colour 204 0 0
        tc3 = Colour 255 255 0
        tc4 = Colour 255 255 255

yellowToGreen :: ColourPointsRGB
yellowToGreen = ColourPointsRGB tc1 tc2 tc3 tc4
        where
        tc1 = Colour 255 255 255
        tc2 = Colour 255 255 0
        tc3 = Colour 102 255 102
        tc4 = Colour 255 255 255

greenToBlue :: ColourPointsRGB
greenToBlue = ColourPointsRGB tc1 tc2 tc3 tc4
        where
        tc1 = Colour 255 255 255
        tc2 = Colour 102 255 102
        tc3 = Colour 51 153 255
        tc4 = Colour 255 255 255

blueToPurple :: ColourPointsRGB
blueToPurple = ColourPointsRGB tc1 tc2 tc3 tc4
        where
        tc1 = Colour 255 255 255
        tc2 = Colour 51 153 255
        tc3 = Colour 153 51 255
        tc4 = Colour 255 255 255

purpleToPink :: ColourPointsRGB
purpleToPink = ColourPointsRGB tc1 tc2 tc3 tc4
        where
        tc1 = Colour 255 255 255
        tc2 = Colour 153 51 255
        tc3 = Colour 255 0 127
        tc4 = Colour 255 255 255

rainbow :: ColourPointsRGB
rainbow = ColourPointsRGB tc1 tc2 tc3 tc4
        where
        tc1 = Colour 204 0 0
        tc2 = Colour 255 255 0
        tc3 = Colour 51 255 51
        tc4 = Colour 51 51 255

rgbToGLrgb :: ColourPointsRGB -> ColourPointsGLfloat
rgbToGLrgb (ColourPointsRGB p1 p2 p3 p4) = ColourPointsGLfloat (convert p1) (convert p2) (convert p3) (convert p4)
            where
                convert (Colour a b c) = Colour (a/255) (b/255) (c/255)

colourListGen :: ColourPointsGLfloat -> [(GLfloat,GLfloat,GLfloat)]
colourListGen (ColourPointsGLfloat c1 c2 c3 c4) =       
        [(r c1 + (co*cdiffR c1 c2),g c1 + (co*cdiffG c1 c2),b c1 + (co*cdiffB c1 c2)) | co <- [0..3333]]
        ++
        [(r c2 + (co*cdiffR c2 c3),g c2 + (co*cdiffG c2 c3),b c2 + (co*cdiffB c2 c3)) | co <- [0..3333]]
        ++
        [(r c3 + (co*cdiffR c3 c4),g c3 + (co*cdiffG c3 c4),b c3 + (co*cdiffB c3 c4)) | co <- [0..3333]]
        where 
                cdiffR x y = (r y - r x) /3333
                cdiffG x y = (g y - g x) /3333
                cdiffB x y = (b y - b x) /3333