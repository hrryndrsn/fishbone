port module Main exposing (Model, Msg(..), cache, init, main, update, view, window)

-- import Browser.Events exposing (onMouseMove)

import Browser
import Html exposing (Html, button, div, h1, img, p, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E
import Svg exposing (circle, rect, svg, text)
import Svg.Attributes exposing (cx, cy, fill, height, r, rx, ry, viewBox, width, x, y)
import Svg.Events exposing (onClick)



-- port out


port cache : E.Value -> Cmd msg



--port In


port window : (E.Value -> msg) -> Sub msg


port mouse : (E.Value -> msg) -> Sub msg



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }



---- MODEL ----


type alias Model =
    { counter : Int
    , window : WindowEvent
    , mouse : MousePos
    , dots : List Dot
    }



-- TYPES


type alias WindowEvent =
    { width : Int
    , height : Int
    }


type alias MousePos =
    { x : Int
    , y : Int
    }


type alias Dot =
    { x : Int
    , y : Int
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { counter = 0
      , window =
            { width = 5
            , height = 5
            }
      , mouse =
            { x = 0
            , y = 0
            }
      , dots = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SendCache
    | Changed E.Value
    | MouseMoved E.Value
    | ClickedSvg
    | AddDot Int Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendCache ->
            ( { model | counter = model.counter + 1 }
            , cache (E.int (model.counter + 1))
            )

        Changed value ->
            let
                pv =
                    parseWindowResize value
            in
            case pv of
                Ok windowEvent ->
                    ( { model
                        | window = windowEvent
                      }
                    , Cmd.none
                    )

                Err error ->
                    let
                        errorMessage =
                            handleDecodeError error
                    in
                    ( model, Cmd.none )

        MouseMoved value ->
            let
                pv =
                    parseMouseMove value
            in
            case pv of
                Ok mousePos ->
                    ( { model
                        | mouse = mousePos
                      }
                    , Cmd.none
                    )

                Err error ->
                    let
                        errorMessage =
                            handleDecodeError error
                    in
                    ( model, Cmd.none )

        ClickedSvg ->
            ( { model | counter = model.counter + 1 }
            , cache (E.int (model.counter + 1))
            )

        AddDot x y ->
            ( { model
                | dots = Dot x y :: model.dots
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


handleDecodeError : Decode.Error -> String
handleDecodeError error =
    case error of
        Decode.Field str errr ->
            "Field Error"

        Decode.Index int err ->
            "Index Error"

        Decode.OneOf errList ->
            "List of Errors"

        Decode.Failure str val ->
            "Failure error"



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ renderSvg
                model
            ]
        ]



--Vew helpers


renderSvg : Model -> Html Msg
renderSvg model =
    let
        stringWidth =
            String.fromInt model.window.width

        stringHeight =
            String.fromInt model.window.height

        sX =
            model.mouse.x
                |> String.fromInt

        sY =
            model.mouse.y
                |> String.fromInt

        sXI =
            model.window.width
                - model.mouse.x
                |> String.fromInt

        sYI =
            model.window.height
                - model.mouse.y
                |> String.fromInt
    in
    svg
        [ width stringWidth
        , height stringHeight
        , viewBox
            ("0 0"
                ++ " "
                ++ stringWidth
                ++ " "
                ++ stringHeight
            )
        , fill "white"
        , onClick (AddDot model.mouse.x model.mouse.y)
        ]
        [ Svg.circle [ cx sX, cy sY, r "50", fill "red" ] []
        , Svg.circle [ cx sXI, cy sY, r "50", fill "black" ] []
        , Svg.circle [ cx sX, cy sYI, r "50", fill "black" ] []
        , Svg.circle [ cx sXI, cy sYI, r "50", fill "black" ] []
        ]


renderDot : Dot -> Svg.Svg msg
renderDot dot =
    let
        sX =
            String.fromInt dot.x

        sY =
            String.fromInt dot.y
    in
    Svg.circle [ cx sX, cy sY, r "50", fill "black" ] []



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ window Changed
        , mouse MouseMoved
        ]



-- DECODERS


parseWindowResize : E.Value -> Result.Result Decode.Error WindowEvent
parseWindowResize value =
    Decode.decodeValue windowEventDecoder value


parseMouseMove : E.Value -> Result.Result Decode.Error MousePos
parseMouseMove value =
    Decode.decodeValue mouseMoveDecoder value


windowEventDecoder : Decode.Decoder WindowEvent
windowEventDecoder =
    Decode.succeed WindowEvent
        |> optional "width" Decode.int 0
        |> optional "height" Decode.int 0


mouseMoveDecoder : Decode.Decoder MousePos
mouseMoveDecoder =
    Decode.succeed MousePos
        |> required "x" Decode.int
        |> required "y" Decode.int
