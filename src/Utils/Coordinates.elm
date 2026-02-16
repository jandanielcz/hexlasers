module Utils.Coordinates exposing (..)

type XY = XY Float Float

x_string : XY -> String
x_string xy = 
    case xy of
        XY x _ -> String.fromFloat x

y_string : XY -> String
y_string xy = 
    case xy of
        XY _ y -> String.fromFloat y

floats_to_XY : (Float, Float) -> XY
floats_to_XY t = 
    XY (Tuple.first t) (Tuple.second t)