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
        rotate = "rotate("++ (String.fromInt (angle + 30 )) ++")"
    in
        Svg.svg [width s, height s, viewBox "0 0 32 32", x (x_string xy), y (y_string xy), transform translate] [
            Svg.path [stroke "black", strokeWidth "3", strokeLinecap "round", d "M 10 22 L 22 22 M 12 26 L 20 26 M 14 30 L 18 30", fill "none", transformOrigin "center center", transform rotate] []
        ]

mirror : XY -> Int -> Int -> Svg.Svg msg
mirror xy size angle = 
    let
        s = String.fromInt size
        translate = "translate(-"++ String.fromFloat ((toFloat size) / 2) ++", -"++ String.fromFloat ((toFloat size) / 2) ++")"
        rotate = "rotate("++ String.fromInt(angle) ++")"
    in
        Svg.svg [width s, height s, viewBox "0 0 32 32", x (x_string xy), y (y_string xy), transform translate] [
            Svg.path [stroke "blue", strokeWidth "3", strokeLinecap "round", d "M 3 9 L 29 23", fill "none", transformOrigin "center center", transform rotate] []
        ]

stone : XY -> Int -> Svg.Svg msg
stone xy size = 
    let
        s = String.fromInt size
        translate = "translate(-"++ String.fromFloat ((toFloat size) / 2) ++", -"++ String.fromFloat ((toFloat size) / 2) ++")"
    in
        Svg.svg [width s, height s, viewBox "0 0 32 32", x (x_string xy), y (y_string xy), transform translate] [
            Svg.path [stroke "black", strokeWidth "3", strokeLinecap "round", strokeLinejoin "bevel", d "M 5 30 L 24 30 L 28 22 L 16 14 L 13 14 L 4 20 Z", fill "gray", transformOrigin "center center"] []
        ]

bulb : XY -> Int -> Bool -> Svg.Svg msg
bulb xy size glows = 
    let
        s = String.fromInt size
        translate = "translate(-"++ String.fromFloat ((toFloat size) / 2) ++", -"++ String.fromFloat ((toFloat size) / 2) ++")"
        bg = if glows then "yellow" else "lightgray"
    in
        Svg.svg [width s, height s, viewBox "0 0 32 32", x (x_string xy), y (y_string xy), transform translate] [
            Svg.circle [stroke "black", strokeWidth "3", cx "16", cy "16", r "12", fill bg, transformOrigin "center center"] []
        ]