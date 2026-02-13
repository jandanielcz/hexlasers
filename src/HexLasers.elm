module HexLasers exposing (main)

import Html as Html
import Html.Attributes as HtmlAttribute
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array
import Tuple
import String
import Debug
import Html.Events exposing (onClick)
import Browser
import Graphics.Hexes
import Utils.Coordinates exposing (floats_to_XY, XY)
import Html.Events exposing (on)
import Html.Attributes exposing (coords)


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
 
type Msg
    = UserClickedCell String
    --| Decrement

type alias Model =
    { size : Int
    , start : (Float, Float)
    , hexes: Array.Array SomeHex
    , field_size : ( Int, Int )
    , clicks: Int
    }

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

calculate_mirror_angle: Angle -> Angle -> Angle
calculate_mirror_angle entry mirror = 
    let
        _ = Debug.log "entry" entry
        _ = Debug.log "mirror" mirror
    in
    if (is_same_or_opposite_angle entry mirror)
        then 
            entry
        else
        case entry of
            QR -> 
                QS
            QS ->
                RS
            RS ->
                RQ
            RQ ->
                SQ
            SQ ->
                SR
            SR ->
                QR

next_angle : Angle -> Angle
next_angle a = 
    case a of 
        QR -> QS
        QS -> RS
        RS -> RQ
        RQ -> SQ
        SQ -> SR
        SR -> QR

trough_angle a =
    case a of
        QR -> RQ
        QS -> SQ
        RS -> SR
        RQ -> QR
        SQ -> QS
        SR -> RS

get_next_angle : SomeHex -> Angle -> Angle
get_next_angle h last_angle =
    --let
        --_ = Debug.log "h" h
        --_ = Debug.log "entry" entry 
        --_ = Debug.log "tr" (trough_angle entry)
    --in
    case h of
        Laser _ a -> 
            a
        Hex _ -> 
            last_angle
        Stone _ -> 
            last_angle 
        Mirror _ a ->
            (calculate_mirror_angle last_angle a)      

goes_laser_trough : SomeHex -> Bool
goes_laser_trough h =
    case h of
        Stone _ -> False
        _ -> True

base_svg_attributes : Model -> List (Attribute msg)
base_svg_attributes m = 
    let
        viewbox_value = "0 0 "++ String.fromInt(Tuple.first(m.field_size)) ++" "++ String.fromInt(Tuple.second(m.field_size))
    in
        [   
        viewBox viewbox_value
        , Svg.Attributes.width (String.fromInt (Tuple.first m.field_size)) 
        , Svg.Attributes.height (String.fromInt (Tuple.second m.field_size))
        ]

hex_to_grid_generator : Model -> Int -> SomeHex -> Svg Msg
hex_to_grid_generator model id hex = 
    case hex of
        Stone c ->
            (stone_hex_svg (hex_center hex model) model.size (String.fromInt id))
        Hex c ->
            (blank_hex_svg (hex_center hex model) model.size (String.fromInt id))
        Laser c a ->
            (laser_hex_svg (hex_center hex model) model.size (String.fromInt id) a)
        Mirror c a ->
            (mirror_hex_svg (hex_center hex model) model.size (String.fromInt id) a)

base_grid : Model -> List (Svg Msg)
base_grid h = Array.toList ((Array.indexedMap (hex_to_grid_generator h) h.hexes))
    

vertical_spacing : Model -> Float
vertical_spacing s = 3/2 * (toFloat s.size)

horizontal_spacing : Model -> Float
horizontal_spacing i = (sqrt 3) * (toFloat i.size)

add_x_y : (Float, Float) -> (Float, Float) -> (Float, Float)
add_x_y base add = 
    ((Tuple.first base) + ( (Tuple.first add)), (Tuple.second base) + ( (Tuple.second add)))

hex_center : SomeHex -> Model -> (Float, Float)
hex_center h s  = 
    let
        coords = get_coords(h)
        x = (sqrt 3) * (toFloat coords.q) * (toFloat s.size)
        x2 = x +  toFloat(coords.r) * ((horizontal_spacing s) / 2)
        y = toFloat(coords.r) * (vertical_spacing s)
        y2 = y
    in
    (add_x_y
        (x2, y2)
        s.start)

path_definition : (Float, Float) -> Int -> String
path_definition t b_size = 
    let
        l_size = b_size
        width = (sqrt 3) * (toFloat l_size)
        height = toFloat b_size * 2
        half_width = width / 2
        half_height = height / 2
        quarter_height = half_height / 2
    in 
    "M "++ (String.fromFloat (Tuple.first t)) ++" "++ (String.fromFloat (Tuple.second t)) ++" " ++
    " m 0 -"++ (String.fromFloat half_height) ++
    " l "++ (String.fromFloat half_width) ++" "++ (String.fromFloat quarter_height) ++
    " l "++ "0 " ++ (String.fromFloat half_height) ++
    " l -"++ (String.fromFloat half_width) ++" "++ (String.fromFloat quarter_height) ++
    " l -"++ (String.fromFloat half_width) ++" -"++ (String.fromFloat quarter_height) ++
    " l "++ "0 -" ++ (String.fromFloat half_height) ++ " Z"

laser_definition : (Float, Float) -> Int -> String
laser_definition t b_size = 
    let
        height = toFloat b_size * 2
        half_height = height / 2 
    in 
    "M "++ (String.fromFloat (Tuple.first t)) ++" "++ (String.fromFloat ((Tuple.second t) + (height / 4))) ++" " ++
    " l 0 -"++ (String.fromFloat (half_height / 2)) ++
    " l 5 5"
    

blank_hex_svg : ( Float, Float ) -> Int -> String -> Svg Msg
blank_hex_svg a_center a_size a_id =   
    Svg.path [ onClick (UserClickedCell a_id), d (path_definition a_center a_size), stroke "none", fill "beige", id a_id] []

stone_hex_svg : ( Float, Float ) -> Int -> String -> Svg Msg
stone_hex_svg a_center a_size a_id =   
    Svg.g [] [
        Svg.path [ onClick (UserClickedCell a_id), d (path_definition a_center a_size), stroke "none", fill "beige", id a_id] []
        , (Graphics.Hexes.stone (floats_to_XY a_center) a_size )
    ]

laser_hex_svg : ( Float, Float ) -> Int -> String -> Angle -> Svg Msg
laser_hex_svg a_center a_size a_id a_angle =   
    Svg.g [] [
        Svg.path [ onClick (UserClickedCell a_id), d (path_definition a_center a_size), stroke "none", fill "beige", id a_id] []
        , (Graphics.Hexes.laser (floats_to_XY a_center) a_size (angle_to_int a_angle) )
    ]

mirror_hex_svg : ( Float, Float ) -> Int -> String -> Angle -> Svg Msg
mirror_hex_svg a_center a_size a_id a_angle =   
    Svg.g [] [
        Svg.path [ onClick (UserClickedCell a_id), d (path_definition a_center a_size), stroke "none", fill "beige", id a_id] []
        , (Graphics.Hexes.mirror (floats_to_XY a_center) a_size (angle_to_int a_angle) )
    ]

draw_laser: SomeHex -> Model -> Svg Msg
draw_laser hex model = 
    Svg.g [] [
        Svg.path [ d (laser_from_hex hex model), stroke "red", strokeWidth "3px", fill "none"] []
    ]


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

find_hex_by_coords : Array.Array SomeHex -> Coords -> Maybe SomeHex
find_hex_by_coords hexes coords = 
    let
        filter n = 
            if get_coords(n) == coords then
                True
                else
                False
        f = Array.filter filter hexes
    in
        List.head (Array.toList f)



hex_way : Model -> List SomeHex -> Angle -> List SomeHex
hex_way model acc last_angle = 

    let
        _ = Debug.log "la" last_angle
    in

    if (List.length acc) > 5 then
        acc
    else
    
    case (List.head (List.reverse acc)) of
        Just from ->
            if (goes_laser_trough (from)) == False then
                acc
            else
            -- finds a hex and adds it to acc list
            case (find_hex_by_coords model.hexes (coords_by_coords_and_angle (get_coords from) (get_next_angle from last_angle))) of
                Just a -> (hex_way model (List.append acc [a]) (get_next_angle from last_angle))
                Nothing ->
                    acc  
        Nothing -> 
            acc


laser_from_hex : SomeHex -> Model -> String
laser_from_hex hex model = 
    let
        start = (hex_center hex model)
        start_s = "M "++ (String.fromFloat (Tuple.first start)) ++ " " ++ (String.fromFloat (Tuple.second start))
        l h m = 
            " L "++ (String.fromFloat (Tuple.first (hex_center h m))) ++" "++ (String.fromFloat (Tuple.second (hex_center h m)))

        hw = (hex_way model [hex] (get_next_angle hex QS))
    in
        --" L 50 50"
        start_s ++ (String.join " " (List.map (\h -> (l h model)) hw))

round1 : Model
round1 = 
    {size = 64
    , start = (64.0, 64.0)
    , hexes = Array.fromList [ Laser (Coords 0 0 0) QS , Hex (Coords 1 0 -1), Mirror (Coords 2 0 -2) QR, Hex (Coords 2 1 -3), Hex (Coords 0 1 -1), Hex (Coords 1 1 -2), Hex (Coords 0 3 -3), Mirror (Coords 0 2 -2) QR ]
    , field_size = ( 800, 600 )
    , clicks = 0
    } 


rotate_hex : Array.Array SomeHex -> String -> Array.Array SomeHex
rotate_hex a index_s = 
    case (String.toInt index_s) of
        Nothing -> a
        Just index ->
            case (Array.get index a) of
                Nothing -> a
                Just s ->
                    case s of
                        Mirror c angle ->
                            Array.set index (Mirror c (next_angle angle)) a
                        _ -> a

update : Msg -> Model -> Model
update msg model =
    case msg of
        UserClickedCell s ->
            let 
                rotated_hexes = (rotate_hex model.hexes s)
            in
                { model | clicks = model.clicks + 1, hexes = rotated_hexes }


view : Model -> Html.Html Msg
view model = 

    let
        filter_laser l = 
            case l of
                Laser _ _ -> True
                _ -> False

        lasers = Array.filter filter_laser model.hexes
        laser_svgs = Array.map (\n -> (draw_laser n model )) lasers
    in
    

    Html.div [ HtmlAttribute.class "hexlasers" ]
        [ Html.h1 [] [ Html.text ("Hexlasers" ++ (String.fromInt model.clicks))]
        , svg (base_svg_attributes model) [
            g [] (base_grid model)
            , g [] (Array.toList laser_svgs)
        ]
        ]

main : Program () Model Msg
main =
    Browser.sandbox
        { init = round1
        , view = view
        , update = update
        }