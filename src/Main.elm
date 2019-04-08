module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrame)
import Element exposing (Attr, Color, Device, DeviceClass(..), Element, alignBottom, alignLeft, alignRight, centerX, centerY, classifyDevice, clip, clipX, column, el, fill, fillPortion, height, htmlAttribute, image, maximum, moveLeft, moveRight, moveUp, onLeft, padding, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font exposing (bold, heavy, size)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Task
import Time exposing (Posix)


main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }


scaled : Int -> Float
scaled =
    Element.modular 16 1.25


scaledInt : Int -> Int
scaledInt =
    scaled >> round


view : Model -> Html msg
view model =
    let
        device =
            classifyDevice model.window

        paddingAmount =
            case device.class of
                Phone ->
                    scaledInt 4

                Tablet ->
                    scaledInt 4

                Desktop ->
                    scaledInt 8

                BigDesktop ->
                    scaledInt 8
    in
    Element.layout
        [ Font.family [ Font.typeface "futura-pt", Font.typeface "sans-serif" ]
        , width fill
        , padding paddingAmount
        , clipX
        ]
        (column [ width (maximum 1366 fill), centerX, spacing (scaledInt 3) ]
            [ header
            , hero device
            ]
        )


header : Element msg
header =
    row []
        [ Element.image [ width (px (scaledInt 13)) ]
            { src = "./logo.svg"
            , description = "Logo"
            }
        ]


hero : Device -> Element msg
hero device =
    let
        buttons =
            case device.class of
                Phone ->
                    column

                Tablet ->
                    row

                Desktop ->
                    row

                BigDesktop ->
                    row
    in
    row
        [ width fill
        , spacing (scaledInt 2)
        ]
        [ buttons [ spacing (scaledInt 1), alignBottom ]
            [ smallCaps device "portfolio"
            , smallCaps device "contact"
            ]
        , splash device
        ]


smallCaps : Device -> String -> Element msg
smallCaps device value =
    let
        fontSize =
            case device.class of
                Phone ->
                    scaledInt 2

                Tablet ->
                    scaledInt 2

                Desktop ->
                    scaledInt 1

                BigDesktop ->
                    scaledInt 1
    in
    el [ Font.bold, Font.size fontSize ] <| text (String.toUpper value)


splash : Device -> Element msg
splash device =
    let
        imageSize =
            case device.class of
                Phone ->
                    px 300

                Tablet ->
                    px 400

                Desktop ->
                    px 400

                BigDesktop ->
                    px 500

        fontSize =
            case device.class of
                Phone ->
                    scaledInt 6

                Tablet ->
                    scaledInt 8

                Desktop ->
                    scaledInt 9

                BigDesktop ->
                    scaledInt 11

        imageShift =
            case device.class of
                Phone ->
                    scaled 10

                Tablet ->
                    scaled 13

                Desktop ->
                    0

                BigDesktop ->
                    0

        textShift =
            case device.class of
                Phone ->
                    scaled 6

                Tablet ->
                    scaled 10

                Desktop ->
                    scaled 12

                BigDesktop ->
                    scaled 12
    in
    image
        [ width imageSize
        , alignRight
        , moveRight imageShift
        , onLeft
            (column
                [ Font.bold
                , moveRight textShift
                , moveUp (scaled 9)
                , Font.size fontSize
                , centerY
                ]
                [ el [ alignRight ] <| text "development"
                , el [ alignRight ] <| text "+ design"
                ]
            )
        ]
        { src = "splash.jpg", description = "" }


type alias Model =
    { window : { width : Int, height : Int } }


type alias Flags =
    { width : Int
    , height : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { window =
            { width = flags.width
            , height = flags.height
            }
      }
    , Cmd.none
    )


type Msg
    = Resize { width : Int, height : Int }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize newWindow ->
            ( { model | window = newWindow }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize (\width height -> Resize { width = width, height = height })
