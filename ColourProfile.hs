module ColourProfile
    (
        Colour,
        ColourProfileRGB
    )
    where

data Colour = Colour Float Float Float
    deriving (Eq,Ord,Show)

data ColourPointsRGB = ColourPointsRGB Colour Colour Colour Colour
    deriving (Eq,Ord,Show)
--Colour control points
-- Warning 
tc1_RGB :: Colour
tc2_RGB :: Colour
tc3_RGB :: Colour
tc4_RGB :: Colour

redToYellow :: ColourPointsRGB
redToYellow = ColourPointsRGB tc1 tc2 tc3 tc4
        where
        tc1_RGB = Colour 255 255 255
        tc2_RGB = Colour 204 0 0
        tc3_RGB = Colour 255 255 0
        tc4_RGB = Colour 255 255 255

yellowToGreen :: ColourPointsRGB
yellowToGreen = ColourPointsRGB tc1 tc2 tc3 tc4
        where
        tc1_RGB = Colour 255 255 255
        tc2_RGB = Colour 255 255 0
        tc3_RGB = Colour 102 255 102
        tc4_RGB = Colour 255 255 255

greenToBlue :: ColourPointsRGB
greenToBlue = ColourPointsRGB tc1 tc2 tc3 tc4
        where
        tc1_RGB = Colour 255 255 255
        tc2_RGB = Colour 102 255 102
        tc3_RGB = Colour 51 153 255
        tc4_RGB = Colour 255 255 255

--Blue to Purple
-- tc1_RGB = Colour 255 255 255
-- tc2_RGB = Colour 51 153 255
-- tc3_RGB = Colour 153 51 255
-- tc4_RGB = Colour 255 255 255

--Purple to pink
tc1_RGB = Colour 255 255 255
tc2_RGB = Colour 153 51 255
tc3_RGB = Colour 255 0 127
tc4_RGB = Colour 255 255 255

--RainBow
-- tc1_RGB = Colour 204 0 0
-- tc2_RGB = Colour 255 255 0
-- tc3_RGB = Colour 51 255 51
-- tc4_RGB = Colour 51 51 255