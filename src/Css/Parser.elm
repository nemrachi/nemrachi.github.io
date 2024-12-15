module Css.Parser exposing (cssStyle)

import Html
import Html.Attributes exposing (style)


cssStyle : String -> List (Html.Attribute msg)
cssStyle css =
    css
        |> String.split ";"
        |> List.filterMap parseAndStyle


parseAndStyle : String -> Maybe (Html.Attribute msg)
parseAndStyle styleDef =
    case String.split ":" styleDef of
        [ key, value ] ->
            Just (style (String.trim key) (String.trim value))

        _ ->
            Nothing
