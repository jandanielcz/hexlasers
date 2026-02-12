module Utils.Svg exposing (transformOrigin)

import Svg exposing (Attribute)
import VirtualDom

transformOrigin : String -> Attribute msg
transformOrigin =
  VirtualDom.attribute "transform-origin"