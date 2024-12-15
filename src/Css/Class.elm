module Css.Class exposing (..)

import Css.Parser exposing (cssStyle)
import Html


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
        height: 100vh;
        """


diagramContainer : List (Html.Attribute msg)
diagramContainer =
    cssStyle
        """
        width: 50%;
        padding: 10px;
        border-left: 1px solid #ccc;
        """
