module Commons.Dict exposing (pop)

import Dict exposing (Dict)


pop : comparable -> Dict comparable v -> ( Maybe v, Dict comparable v )
pop key dict =
    case Dict.get key dict of
        Just value ->
            ( Just value, Dict.remove key dict )

        Nothing ->
            ( Nothing, dict )
