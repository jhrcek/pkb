module Main exposing (main)

import Html exposing (Html, details, div, input, label, summary, text)
import Html.Attributes exposing (checked, class, type_)
import Html.Events exposing (onCheck, onInput)
import Http
import Markdown
import Navigation
import Note exposing (Note)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


type alias Model =
    { notes : List Note
    , searchFilter : SearchFilter
    }


type SearchFilter
    = SearchFilter String MatchType


type MatchType
    = TitleOnly
    | TitleAndBody


init : Navigation.Location -> ( Model, Cmd Msg )
init _ =
    ( { notes = []
      , searchFilter = SearchFilter "" TitleAndBody
      }
    , Http.send NotesReceived (Http.get "/notes" Note.notesDecoder)
    )


type Msg
    = UrlChange Navigation.Location
    | NotesReceived (Result Http.Error (List Note))
    | ChangeFilterText String
    | ChangeFilterMatchType MatchType


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange _ ->
            model ! []

        NotesReceived result ->
            case result of
                Err e ->
                    let
                        _ =
                            Debug.log "Error getting note " e
                    in
                    model ! []

                Ok notes ->
                    { model | notes = notes } ! []

        ChangeFilterText filterText ->
            { model | searchFilter = setFilterText filterText model.searchFilter } ! []

        ChangeFilterMatchType matchType ->
            { model | searchFilter = setFilterMatchType matchType model.searchFilter } ! []


setFilterText : String -> SearchFilter -> SearchFilter
setFilterText newText (SearchFilter _ mt) =
    SearchFilter newText mt


setFilterMatchType : MatchType -> SearchFilter -> SearchFilter
setFilterMatchType matchType (SearchFilter filterText _) =
    SearchFilter filterText matchType


getMatchType : SearchFilter -> MatchType
getMatchType (SearchFilter _ matchType) =
    matchType


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", onInput ChangeFilterText ] []
        , label []
            [ input
                [ type_ "checkbox"
                , checked (getMatchType model.searchFilter == TitleOnly)
                , onCheck
                    (\b ->
                        if b then
                            ChangeFilterMatchType TitleOnly
                        else
                            ChangeFilterMatchType TitleAndBody
                    )
                ]
                []
            , text "Search title only"
            ]
        , div [] <| List.map viewNote <| List.filter (noteMatchesFilter model.searchFilter) model.notes
        ]


viewNote : Note -> Html a
viewNote note =
    details []
        [ summary [ class "note-title" ] [ text note.nTitle ]
        , Markdown.toHtml [ class "note-body" ] note.nBody
        ]


noteMatchesFilter : SearchFilter -> Note -> Bool
noteMatchesFilter (SearchFilter filterText matchType) note =
    case matchType of
        TitleOnly ->
            String.contains filterText note.nTitle

        TitleAndBody ->
            String.contains filterText note.nTitle
                || String.contains filterText note.nBody
