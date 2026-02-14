module HexLasers exposing (main )

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
import Utils.Coordinates exposing (floats_to_XY)
import Html.Attributes exposing (coords)
import Utils.Hexgrid exposing (..)
import Rounds exposing (..)


 
type Msg
    = UserClickedCell String
    --| Decrement



-- Mirror O, 1, 2 are same as 3, 4, 5 - mirror are two sided
calculate_mirror_angle: Angle -> Angle -> Angle
calculate_mirror_angle last_angle mirror_angle = 
    let
        in_angle = (trough_angle last_angle)
        directly_facing = [mirror_angle, (trough_angle mirror_angle)]
        --_ = Debug.log "in" (in_angle)   
        --_ = Debug.log "rm" (directly_facing) 
        diff = (angle_to_ord in_angle) - (angle_to_ord mirror_angle)
        --_ = Debug.log "diff" diff
    in
        -- if mirror is diretly facing in wall then return to that 
        if (List.member in_angle directly_facing) then
            in_angle
        else
        -- not shure how this work, experimented
        if (List.member diff [1, 4, -2]) then
            ord_to_angle ((angle_to_ord in_angle) - 2)
        else
        if (List.member diff [-1, 2, -4, 5]) then
            ord_to_angle ((angle_to_ord in_angle) + 2)
        else
            last_angle

                

get_next_angle : SomeHex -> Angle -> Angle
get_next_angle h last_angle =
    
    case h of
        -- laser has own fixed angle
        Laser _ a -> 
            a
        -- mirror is calculated
        Mirror _ mirror_angle ->
            (calculate_mirror_angle last_angle mirror_angle)     
        -- empty hex keeps angle same, bulb, stone does not matter
        _ ->
            last_angle

goes_laser_trough : SomeHex -> Bool
goes_laser_trough h =
    case h of
        Stone _ -> False
        Bulb _ _ -> False
        Laser _ _ -> True
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
        Bulb c g ->
            (bulb_hex_svg (hex_center hex model) model.size (String.fromInt id) g)

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
    

blank_hex_svg : ( Float, Float ) -> Int -> String -> Svg Msg
blank_hex_svg a_center a_size a_id =   
    Svg.path [ onClick (UserClickedCell a_id), d (path_definition a_center a_size), stroke "none", fill "beige", id a_id] []

stone_hex_svg : ( Float, Float ) -> Int -> String -> Svg Msg
stone_hex_svg a_center a_size a_id =   
    Svg.g [] [
        Svg.path [ onClick (UserClickedCell a_id), d (path_definition a_center a_size), stroke "none", fill "beige", id a_id] []
        , (Graphics.Hexes.stone (floats_to_XY a_center) a_size )
    ]

bulb_hex_svg : ( Float, Float ) -> Int -> String -> Bool -> Svg Msg
bulb_hex_svg a_center a_size a_id glows =   
    Svg.g [] [
        Svg.path [ onClick (UserClickedCell a_id), d (path_definition a_center a_size), stroke "none", fill "beige", id a_id] []
        , (Graphics.Hexes.bulb (floats_to_XY a_center) a_size glows)
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



hex_way : Array.Array SomeHex -> List SomeHex -> Angle -> List SomeHex
hex_way hexes acc last_angle = 
    if (List.length acc) > 99 then
        acc
    else
    
    case (List.head (List.reverse acc)) of
        Just from ->
            if (goes_laser_trough from) == False then
                acc
            else
            -- finds a hex and adds it to acc list
            case (find_hex_by_coords hexes (coords_by_coords_and_angle (get_coords from) (get_next_angle from last_angle))) of
                Just a -> (hex_way hexes (List.append acc [a]) (get_next_angle from last_angle))
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

        hw = (hex_way model.hexes [hex] (get_next_angle hex RS))
        --_ = Debug.log "hw" hw
    in
        --" L 50 50"
        start_s ++ (String.join " " (List.map (\h -> (l h model)) hw))




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

check_bulbs : Array.Array SomeHex -> Array.Array SomeHex
check_bulbs hexes = 
    let
        filter_laser l = 
            case l of
                Laser _ _ -> True
                _ -> False
        lasers = Array.filter filter_laser hexes
        hw = case (List.head (Array.toList lasers)) of
            Just a -> (hex_way hexes [a] (get_next_angle a RS))
            Nothing -> []
        _ = Debug.log "hw" hw

        glow_bulbs a =
            if (List.member a hw) then
                case a of
                    Bulb c _ ->
                        Bulb c True
                    _ -> a
            else
            a

        glown_hexes = Array.map glow_bulbs hexes
    in
    
    glown_hexes

update : Msg -> Model -> Model
update msg model =
    case msg of
        UserClickedCell s ->
            let 
                rotated_hexes = (rotate_hex model.hexes s)
                check_bulbs_result = (check_bulbs rotated_hexes)
            in
                { model | clicks = model.clicks + 1, hexes = check_bulbs_result }


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