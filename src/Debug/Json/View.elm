module Debug.Json.View exposing (program, viewValue, viewString)

import Json.Encode
import Json.Decode
import Html exposing (Html)
import Html.Lazy
import Html.Attributes
import Html.Events
import Http
import Http.Error.View
import Markdown exposing (defaultOptions)


type Msg a
    = WaitingForValue
    | DisplayValue Json.Decode.Value
    | FailedToLoadUrl Http.Error
    | ChangeUrl String
    | Resend


type alias Model a =
    { url : String
    , decoder : Json.Decode.Decoder a
    , response : Maybe (Result Http.Error Json.Decode.Value)
    }


update : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update msg model =
    case msg of
        WaitingForValue ->
            ( model, Cmd.none )

        DisplayValue value ->
            ( { model | response = Just <| Ok value }, Cmd.none )

        FailedToLoadUrl error ->
            ( { model | response = Just <| Err error }, Cmd.none )

        ChangeUrl url ->
            ( { model | url = url, response = Nothing }
            , Cmd.none
            )

        Resend ->
            ( model
            , Http.get model.url Json.Decode.value
                |> Http.send handleResponse
            )


handleResponse : Result Http.Error Json.Decode.Value -> Msg a
handleResponse result =
    case result of
        Err error ->
            FailedToLoadUrl error

        Ok json ->
            DisplayValue json


program : String -> Json.Decode.Decoder a -> Program Never (Model a) (Msg a)
program url decoder =
    Html.program
        { init =
            ( { url = url, decoder = decoder, response = Nothing }
            , Http.get url Json.Decode.value
                |> Http.send handleResponse
            )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


viewResponse : Model a -> Html (Msg a)
viewResponse model =
    case model.response of
        Nothing ->
            Html.div
                []
                [ Html.text <| "Waiting on response from.." ++ model.url ]

        Just response ->
            case response of
                Err error ->
                    Http.Error.View.view error

                Ok json ->
                    viewValue model.decoder json


viewUrl : Model a -> Html (Msg a)
viewUrl model =
    Html.div
        [ Html.Attributes.style [ ( "padding-top", "1px" ) ] ]
        [ Html.input
            [ Html.Attributes.type_ "text"
            , Html.Events.onInput ChangeUrl
            , Html.Attributes.value model.url
            , Html.Attributes.style [ ( "width", "400px" ) ]
            ]
            []
        , Html.button [ Html.Events.onClick Resend ] [ Html.text "Go!" ]
        ]


unit =
    ()


view : Model a -> Html (Msg a)
view model =
    Html.div
        [ Html.Attributes.style
            [ ( "padding-left", "25px" )
            , ( "padding-top", "50px" )
            ]
        ]
        [ Html.text "Currently testing the URL :"
        , viewUrl model
        , viewResponse model
        , Html.div
            []
            [ Html.node "link"
                [ Html.Attributes.property "rel" (Json.Encode.string "stylesheet")
                , Html.Attributes.property "href" (Json.Encode.string "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css")
                ]
                []
            , Html.node "script"
                --[]
                [ Html.Attributes.src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
                , Html.Attributes.attribute "onload" "setTimeout(function() {window.hljs.initHighlighting()}, 100);"
                ]
                []
            ]
        ]


viewValue : Json.Decode.Decoder a -> Json.Decode.Value -> Html msg
viewValue decoder value =
    case Json.Decode.decodeValue decoder value of
        Err message ->
            Html.div
                []
                [ Html.div [] [ Html.text "I failed to decode the JSON value" ]
                , Html.div [] [ Html.text <| "I was using the value: " ]
                , Markdown.toHtml [] ("```Python\n" ++ toString value ++ "```")
                , Html.div [] [ Html.text "but I got the error: " ]
                , Markdown.toHtml [] ("```Python\n" ++ message ++ "```")
                ]

        Ok parsedValue ->
            Html.div
                []
                [ Html.div [] [ Html.text "Correctly parsed!" ]
                , Html.div [] [ Html.text "And so I got the value: " ]
                , Markdown.toHtml
                    []
                    ("```Python\n" ++ toString parsedValue ++ "```")
                ]


viewString : Json.Decode.Decoder a -> String -> Html msg
viewString decoder value =
    case Json.Decode.decodeString decoder value of
        Err message ->
            Html.div
                []
                [ Html.div [] [ Html.text "I failed to decode the JSON value" ]
                , Html.div [] [ Html.text <| "I was using the value: " ]
                , Markdown.toHtml [] ("```Python\n" ++ toString value ++ "```")
                , Html.div [] [ Html.text "but I got the error: " ]
                , Markdown.toHtml [] ("```Python\n" ++ message ++ "```")
                ]

        Ok parsedValue ->
            Html.div
                []
                [ Html.div [] [ Html.text "Correctly parsed" ]
                , Html.div [] [ Html.text "And so I got the value: " ]
                , Markdown.toHtml
                    []
                    ("```Python\n" ++ toString parsedValue ++ "```")
                ]
