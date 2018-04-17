module Main exposing (main)

import Html exposing (Html, button, details, div, input, label, summary, text)
import Html.Attributes exposing (checked, class, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
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
    | SetSearchQuery String
    | SearchSettingsChanged SearchSettings
    | ClearSearchQuery


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updatePure msg model, Cmd.none )


updatePure : Msg -> Model -> Model
updatePure msg model =
    case msg of
        UrlChange _ ->
            model

        NotesReceived nodesWebData ->
            { model | notes = nodesWebData }

        SetSearchQuery queryString ->
            { model | searchQuery = SearchQuery queryString }

        ClearSearchQuery ->
            { model | searchQuery = SearchQuery "" }

        SearchSettingsChanged newSettings ->
            { model | searchSettings = newSettings }


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
            loadedNotes
                |> List.filter (noteMatchesQuery model.searchSettings model.searchQuery)
                |> List.sortBy .nTitle
                |> List.map viewNote
                |> div []


view : Model -> Html Msg
view model =
    div []
        [ searchBar model.searchSettings model.searchQuery
        , viewNotes model
        ]


searchBar : SearchSettings -> SearchQuery -> Html Msg
searchBar searchSettings (SearchQuery queryString) =
    div []
        [ input [ type_ "text", onInput SetSearchQuery, value queryString ] []
        , button [ onClick ClearSearchQuery ] [ text "Clear" ]
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


noteMatchesQuery : SearchSettings -> SearchQuery -> Note -> Bool
noteMatchesQuery searchSettings (SearchQuery queryString) note =
    case searchSettings of
        MatchTitle ->
            String.contains queryString note.nTitle

        MatchTitleAndBody ->
            String.contains queryString note.nTitle
                || String.contains queryString note.nBody
