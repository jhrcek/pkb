module Main exposing (main)

import Html exposing (Html, details, div, input, label, summary, text)
import Html.Attributes exposing (checked, class, type_)
import Html.Events exposing (onCheck, onInput)
import Http
import Markdown
import Navigation
import Note exposing (Note)
import RemoteData exposing (WebData)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


type alias Model =
    { notes : WebData (List Note)
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
    ( { notes = RemoteData.Loading
      , searchSettings = MatchTitleAndBody
      , searchQuery = SearchQuery ""
      }
    , Http.get "/notes" Note.notesDecoder
        |> RemoteData.sendRequest
        |> Cmd.map NotesReceived
    )


type Msg
    = UrlChange Navigation.Location
    | NotesReceived (WebData (List Note))
    | ChangeFilterText String
    | SearchSettingsChanged SearchSettings


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange _ ->
            model ! []

        NotesReceived nodesWebData ->
            { model | notes = nodesWebData } ! []

        ChangeFilterText filterText ->
            { model | searchQuery = SearchQuery filterText } ! []

        SearchSettingsChanged newSettings ->
            { model | searchSettings = newSettings } ! []


viewNotes : Model -> Html Msg
viewNotes model =
    case model.notes of
        RemoteData.Loading ->
            text "Loading ..."

        RemoteData.NotAsked ->
            text "We shoulnd never end up in 'NotAsked' state"

        RemoteData.Failure error ->
            text ("Error loading notes" ++ toString error)

        RemoteData.Success loadedNotes ->
            div [] <| List.map viewNote <| List.filter (noteMatchesFilter model.searchSettings model.searchQuery) loadedNotes


view : Model -> Html Msg
view model =
    div []
        [ searchBar model.searchSettings
        , viewNotes model
        ]


searchBar : SearchSettings -> Html Msg
searchBar searchSettings =
    div []
        [ input [ type_ "text", onInput ChangeFilterText ] []
        , label []
            [ input
                [ type_ "checkbox"
                , checked (searchSettings == MatchTitle)
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
