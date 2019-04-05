module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrame)
import Element exposing (Attr, Color, Element, alignBottom, alignRight, column, el, fill, fillPortion, height, htmlAttribute, padding, paragraph, px, text, width)
import Element.Background as Background
import Element.Font as Font exposing (bold, heavy, size)
import Html exposing (Html)
import Html.Attributes
import Task
import Time exposing (Posix)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { scrollPosition : Float }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { scrollPosition = 0
      }
    , Cmd.none
    )


type Msg
    = AnimationFrame Posix
    | ViewportChange Viewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame _ ->
            ( model, Task.perform ViewportChange getViewport )

        ViewportChange viewport ->
            ( { model | scrollPosition = viewport.viewport.y }, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout [ height (px 1500) ] (container model)


orange : Color
orange =
    Element.rgb255 206 73 0


white : Color
white =
    Element.rgb 255 255 255


rotate : Float -> Html.Attribute msg
rotate percent =
    let
        x =
            "rotate3d(1, 0, 0, " ++ (String.fromInt <| round (linearInterpolation 45 0 percent)) ++ "deg)"

        y =
            "rotate3d(0, 1, 0, " ++ (String.fromInt <| round (linearInterpolation 15 0 percent)) ++ "deg)"

        z =
            "rotate3d(0, 0, 1, -" ++ (String.fromInt <| round (linearInterpolation 35 0 percent)) ++ "deg)"

        scale =
            "scale(" ++ String.fromFloat (linearInterpolation 1.5 1 percent) ++ ")"
    in
    Html.Attributes.style "transform" (x ++ " " ++ y ++ " " ++ z ++ " " ++ scale)


linearInterpolation : Float -> Float -> Float -> Float
linearInterpolation start end percent =
    start + (end - start) * percent


container : Model -> Element Msg
container model =
    column
        [ width fill
        , htmlAttribute
            (rotate (clamp 0 400 model.scrollPosition / 400))
        , htmlAttribute <| Html.Attributes.style "transition" "transform 0.05s"
        ]
        [ Element.row [ width fill, height (px 500) ]
            [ el [ width (fillPortion 1), height fill, Background.color white ]
                (column
                    [ alignRight, alignBottom, padding 40, size 50, heavy ]
                    [ el [ alignRight ] (text "designer")
                    , el [ alignRight ] (text "+ developer")
                    ]
                )
            , el [ width (fillPortion 2), height fill, Background.color orange ] Element.none
            ]
        , Element.row [ width fill, height (px 500) ]
            [ el [ width (fillPortion 1), height fill, Background.color orange ] Element.none
            , el [ width (fillPortion 2), height fill, Background.color white ] Element.none
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrame AnimationFrame
