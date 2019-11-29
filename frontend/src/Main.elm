module Main exposing (main)

import Browser
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Http exposing (Error(..))
import Notes exposing (Notes)
import RemoteData exposing (RemoteData(..), WebData)
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        }


type alias Model =
    { alert : Maybe String
    , data : WebData Notes.Model
    }


type Msg
    = NotesReceived (WebData Notes)
    | NoteSaved (WebData ())
    | NotesMsg Notes.Msg
    | UrlChange Url
    | UrlRequest Browser.UrlRequest
    | DismissAlert


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { alert = Nothing
      , data = RemoteData.Loading
      }
    , Notes.getNotes NotesReceived
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NotesReceived notesWebData ->
            ( { model | data = RemoteData.map Notes.init notesWebData }
            , Cmd.none
            )

        NoteSaved webData ->
            case webData of
                Failure httpError ->
                    ( { model | alert = Just (httpErrorToString httpError) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        NotesMsg notesMsg ->
            case RemoteData.map (Notes.update NoteSaved notesMsg) model.data of
                Success ( newNotes, cmd ) ->
                    ( { model | data = Success newNotes }, cmd )

                _ ->
                    ( model, Cmd.none )

        DismissAlert ->
            ( { model | alert = Nothing }
            , Cmd.none
            )

        UrlChange _ ->
            ( model, Cmd.none )

        UrlRequest _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        alertView_ =
            case model.alert of
                Nothing ->
                    Html.text ""

                Just alert ->
                    alertView alert
    in
    { title = "Personal Knowledge Base"
    , body =
        [ alertView_
        , dataView model.data
        ]
    }


dataView : WebData Notes.Model -> Html Msg
dataView data =
    case data of
        RemoteData.Loading ->
            Html.text "Loading notes ..."

        RemoteData.NotAsked ->
            Html.text "We should never end up in 'NotAsked' state"

        RemoteData.Failure error ->
            Html.text ("Failed to load noten" ++ httpErrorToString error)

        RemoteData.Success notesModel ->
            Html.map NotesMsg <| Notes.view notesModel


alertView : String -> Html Msg
alertView alertText =
    Html.div [ Attr.class "alert" ]
        [ Html.text alertText
        , Html.span
            [ Html.Events.onClick DismissAlert
            , Attr.class "alert-close"
            ]
            [ Html.text "×" ]
        ]


httpErrorToString : Http.Error -> String
httpErrorToString error =
    "Http Error : "
        ++ (case error of
                BadUrl x ->
                    "Bad Url : " ++ x

                Timeout ->
                    "Time"

                NetworkError ->
                    "NetworkError"

                BadStatus statusCode ->
                    "BadStatus " ++ String.fromInt statusCode

                BadBody bodyStr ->
                    "BadBody " ++ bodyStr
           )
