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
    , searchSettings : SearchSettings
    , searchQuery : SearchQuery
    }


type SearchQuery
    = SearchQuery String


type SearchSettings
    = MatchTitle
    | MatchTitleAndBody


init : Navigation.Location -> ( Model, Cmd Msg )
init _ =
    ( { notes = []
      , searchSettings = MatchTitleAndBody
      , searchQuery = SearchQuery ""
      }
    , Http.send NotesReceived (Http.get "/notes" Note.notesDecoder)
    )


type Msg
    = UrlChange Navigation.Location
    | NotesReceived (Result Http.Error (List Note))
    | ChangeFilterText String
    | SearchSettingsChanged SearchSettings


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange _ ->
            model ! []

        NotesReceived result ->
            case result of
                Err e ->
                    -- TODO handle notes loading error
                    let
                        _ =
                            Debug.log "Error getting note " e
                    in
                    model ! []

                Ok notes ->
                    { model | notes = notes } ! []

        ChangeFilterText filterText ->
            { model | searchQuery = SearchQuery filterText } ! []

        SearchSettingsChanged newSettings ->
            { model | searchSettings = newSettings } ! []


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", onInput ChangeFilterText ] []
        , label []
            [ input
                [ type_ "checkbox"
                , checked (model.searchSettings == MatchTitle)
                , onCheck
                    (\b ->
                        SearchSettingsChanged
                            (if b then
                                MatchTitle
                             else
                                MatchTitleAndBody
                            )
                    )
                ]
                []
            , text "Search title only"
            ]
        , div [] <| List.map viewNote <| List.filter (noteMatchesFilter model.searchSettings model.searchQuery) model.notes
        ]


viewNote : Note -> Html a
viewNote note =
    details []
        [ summary [ class "note-title" ] [ text note.nTitle ]
        , Markdown.toHtml [ class "note-body" ] note.nBody
        ]


noteMatchesFilter : SearchSettings -> SearchQuery -> Note -> Bool
noteMatchesFilter searchSettings (SearchQuery filterText) note =
    case searchSettings of
        MatchTitle ->
            String.contains filterText note.nTitle

        MatchTitleAndBody ->
            String.contains filterText note.nTitle
                || String.contains filterText note.nBody
