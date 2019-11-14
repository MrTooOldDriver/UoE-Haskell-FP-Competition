module ColourProfile
    (
        Colour,
        redToYellow,
        yellowToGreen,
        greenToBlue,
        blueToPurple,
        purpleToPink,
        rainbow
    )
    where

data Colour = Colour Float Float Float
    deriving (Eq,Ord,Show)

data ColourPointsRGB = ColourPointsRGB Colour Colour Colour Colour
    deriving (Eq,Ord,Show)

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