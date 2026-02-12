module Graphics.Hexes exposing (..)

import Svg
import Svg.Attributes exposing (..)
import Utils.Coordinates exposing (..)
import Utils.Svg exposing (transformOrigin)

laser : XY -> Int -> Int -> Svg.Svg msg
laser xy size angle = 
    let
        s = String.fromInt size
        translate = "translate(-"++ String.fromFloat ((toFloat size) / 2) ++", -"++ String.fromFloat ((toFloat size) / 2) ++")"
        rotate = "rotate("++ String.fromInt(angle) ++")"
    in
        Svg.svg [width s, height s, viewBox "0 0 32 32", x (x_string xy), y (y_string xy), transform translate] [
            Svg.path [stroke "black", strokeWidth "3", strokeLinecap "round", d "M 10 22 L 22 22 M 12 26 L 20 26 M 14 30 L 18 30", fill "none", transformOrigin "center center", transform rotate] []
        ]