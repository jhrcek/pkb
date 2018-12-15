module Highlight exposing (highlight)

import Html exposing (Html)
import Html.Attributes exposing (class)


highlight : String -> String -> Html a
highlight part whole =
    if String.isEmpty part then
        Html.text whole

    else
        let
            highlightedPart =
                Html.span [ class "highlight" ] [ Html.text part ]
        in
        String.split part whole
            |> List.map Html.text
            |> List.intersperse highlightedPart
            |> Html.span []
