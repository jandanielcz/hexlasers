module Rounds exposing (..)

import Array
import Utils.Hexgrid exposing (..)

type alias Model =
    { size : Int
    , start : (Float, Float)
    , hexes: Array.Array SomeHex
    , field_size : ( Int, Int )
    , clicks: Int
    }

round1 : Model
round1 = 
    {size = 64
    , start = (64.0, 64.0)
    , hexes = Array.fromList [ 
        Laser (Coords 0 0 0) QS , Hex (Coords 1 0 -1), Mirror (Coords 2 0 -2) SQ, Bulb (Coords 3 0 -3) False, Stone (Coords 4 0 -4)
        , Hex (Coords 0 1 -1), Hex (Coords 1 1 -2), Mirror (Coords 2 1 -3) SQ, Hex (Coords 3 1 -4), Mirror (Coords 4 1 -5) QS
        , Mirror (Coords 0 2 -2) QR, Hex (Coords 1 2 -3), Hex (Coords 2 2 -4), Hex (Coords 3 2 -5)
        , Mirror (Coords 0 3 -3) RS, Hex (Coords 1 3 -4), Hex (Coords 2 3 -5)
        , Hex (Coords 0 4 -4), Hex (Coords 1 4 -5), Mirror (Coords 2 4 -6) RS
        , Mirror (Coords 0 5 -5) SR, Mirror (Coords 1 5 -6) SR
        ]
    , field_size = ( 800, 600 )
    , clicks = 0
    } 