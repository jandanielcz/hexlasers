module Utils.Hexgrid exposing (..)

type alias Coords
    = { q: Int
    , r: Int
    , s: Int
    }

type Angle 
    = QR 
    | QS 
    | RS
    | RQ
    | SQ
    | SR
    
type SomeHex 
    = Hex Coords
    | Stone Coords
    | Laser Coords Angle
    | Mirror Coords Angle
    | Bulb Coords Bool

angle_to_int : Angle -> Int
angle_to_int a = 
    case a of 
        QR -> 0
        QS -> 60
        RS -> 120
        RQ -> 180
        SQ -> 240
        SR -> 300

get_coords : SomeHex -> Coords
get_coords s = 
    case s of
        Hex c ->
            c
        Stone c ->
            c
        Laser c _ ->
            c
        Mirror c _ ->
            c
        Bulb c _ ->
            c

angle_to_ord : Angle -> Int
angle_to_ord angle = 
    case angle of
        QR -> 0
        QS -> 1
        RS -> 2
        RQ -> 3
        SQ -> 4
        SR -> 5

ord_to_angle : Int -> Angle
ord_to_angle i = 
    let
        mod = (modBy 6 i)
    in
    case mod of
        0 -> QR
        1 -> QS
        2 -> RS
        3 -> RQ
        4 -> SQ
        5 -> SR
        _ -> QR

is_same_or_opposite_angle : Angle -> Angle -> Bool
is_same_or_opposite_angle entry mirror = 
    if (entry == mirror) then
        True
    else
        case (entry, mirror) of
            (QR, RQ) -> 
                True
            (RQ, QR) ->
                True
            (QS, SQ) -> 
                True
            (SQ, QS) ->
                True
            (SR, RS) -> 
                True
            (RS, SR) ->
                True
            (_, _) -> False

next_angle : Angle -> Angle
next_angle a = 
    case a of 
        QR -> QS
        QS -> RS
        RS -> RQ
        RQ -> SQ
        SQ -> SR
        SR -> QR

trough_angle : Angle -> Angle
trough_angle a =
    case a of
        QR -> RQ
        QS -> SQ
        RS -> SR
        RQ -> QR
        SQ -> QS
        SR -> RS

coords_by_coords_and_angle : Coords -> Angle -> Coords
coords_by_coords_and_angle coords a = 
    case a of
        QR -> 
            {coords | q = coords.q + 1, r = coords.r - 1 }
        QS -> 
            {coords | q = coords.q + 1, s = coords.s - 1 }
        RS -> 
            {coords | r = coords.r + 1, s = coords.s - 1 }
        RQ -> 
            {coords | r = coords.r + 1, q = coords.q - 1 }
        SQ -> 
            {coords | s = coords.s + 1, q = coords.q - 1 }
        SR -> 
            {coords | s = coords.s + 1, r = coords.r - 1 }