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

type Angle = Angle Int
    
type SomeHex 
    = Hex Coords
    | Stone Coords
    | Laser Coords Angle
 
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
        Angle b -> b

get_coords : SomeHex -> Coords
get_coords s = 
    case s of
        Hex c ->
            c
        Stone c ->
            c
        Laser c _ ->
            c
            

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
    Svg.path [ onClick (UserClickedCell a_id), d (path_definition a_center a_size), stroke "black", fill "black", id a_id] []

laser_hex_svg : ( Float, Float ) -> Int -> String -> Angle -> Svg Msg
laser_hex_svg a_center a_size a_id a_angle =   
    Svg.g [] [
        Svg.path [ onClick (UserClickedCell a_id), d (path_definition a_center a_size), stroke "none", fill "beige", id a_id] []
        , (Graphics.Hexes.laser (floats_to_XY a_center) a_size (angle_to_int a_angle) )
    ]

draw_laser: SomeHex -> Model -> Svg Msg
draw_laser hex model = 
    Svg.g [] [
        Svg.path [ d (laser_from_hex hex model), stroke "red", strokeWidth "3px", fill "none"] []
    ]

next_hex : SomeHex -> Model -> SomeHex
next_hex source model = 
    Hex (Coords 0 3 -3)

coords_by_coords_and_angle : Coords -> Angle -> Coords
coords_by_coords_and_angle coords a = 
    case a of
        Angle 45 -> 
            {coords | q = coords.q + 1, r = coords.r - 1 }
        _ -> coords

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

hex_way model acc = 
    let
        -- TODO Here use coords_by_coords_and_angle to get coords find_hex_by_coords to find hex or Nothing
        found = case acc of
            [] -> 
                Just (Hex (Coords 0 3 -3))
            _ -> Nothing
    in
        case found of
            Just h -> 
                hex_way model (List.append acc [h])
            Nothing ->
                acc

laser_from_hex : SomeHex -> Model -> String
laser_from_hex hex model = 
    let
        start = (hex_center hex model)
        start_s = "M "++ (String.fromFloat (Tuple.first start)) ++ " " ++ (String.fromFloat (Tuple.second start))
            
        hexes_in_path = List.append [ (next_hex hex model)] [hex]
        l h m = 
            " L "++ (String.fromFloat (Tuple.first (hex_center h m))) ++" "++ (String.fromFloat (Tuple.second (hex_center h m)))

        hw = (hex_way model [])
        _ = Debug.log "hw" hw
    in
        --" L 50 50"
        start_s ++ (String.join " " (List.map (\h -> (l h model)) hexes_in_path))

round1 : Model
round1 = 
    {size = 64
    , start = (64.0, 64.0)
    , hexes = Array.fromList [ Laser (Coords 0 0 0) (Angle 90) , Hex (Coords 1 0 -1), Hex (Coords 2 0 -2), Hex (Coords 0 1 -1), Hex (Coords 0 3 -3), Stone (Coords 0 2 -2) ]
    , field_size = ( 800, 600 )
    , clicks = 0
    } 


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserClickedCell s ->
            let 
                _ = Debug.log "UserClickedCell" s
            in
                { model | clicks = model.clicks + 1 }


view : Model -> Html.Html Msg
view model = 

    let
        _ = Debug.log "model" model
        -- _ = Debug.log "vs" (vertical_spacing size)

        filter_laser l = 
            case l of
                Laser _ _ -> True
                _ -> False

        lasers = Array.filter filter_laser model.hexes
        laser_svgs = Array.map (\n -> (draw_laser n model )) lasers
        _ = Debug.log "lasers" laser_svgs
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