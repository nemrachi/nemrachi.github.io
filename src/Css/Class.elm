module Css.Class exposing (diagramContainer, flexContainer, noSelectSvgAttributes, texrAreaContainer, textArea)

import Css.Parser exposing (cssStyle)
import Html
import Svg
import Svg.Attributes


flexContainer : List (Html.Attribute msg)
flexContainer =
    cssStyle "display: flex;"


texrAreaContainer : List (Html.Attribute msg)
texrAreaContainer =
    cssStyle
        """
        width: 50%;
        padding: 10px;
        """


textArea : List (Html.Attribute msg)
textArea =
    cssStyle
        """
        width: 100%;
        height: 97vh;
        """


diagramContainer : List (Html.Attribute msg)
diagramContainer =
    cssStyle
        """
        width: 50%;
        padding: 10px;
        border-left: 1px solid #ccc;
        """


noSelectSvgAttributes : List (Svg.Attribute msg)
noSelectSvgAttributes =
    [ Svg.Attributes.style "-webkit-user-select: none"
    , Svg.Attributes.style "-moz-user-select: none"
    , Svg.Attributes.style "-ms-user-select: none"
    , Svg.Attributes.style "user-select: none"
    ]
