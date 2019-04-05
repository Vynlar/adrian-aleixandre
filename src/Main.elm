module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrame)
import Element exposing (Attr, Color, Element, alignBottom, alignRight, clip, clipX, column, el, fill, fillPortion, height, htmlAttribute, padding, paragraph, px, text, width)
import Element.Background as Background
import Element.Font as Font exposing (bold, heavy, size)
import Html exposing (Html)
import Html.Attributes exposing (style)
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
    Element.layout [ height (px 1500), clipX ] (layout model)


orange : Color
orange =
    Element.rgb255 206 73 0


white : Color
white =
    Element.rgb 255 255 255


rotate : Float -> Element.Attribute msg
rotate percent =
    let
        lerp : Float -> Float -> String
        lerp start end =
            String.fromInt <| round (linearInterpolation start end percent)

        x =
            "rotate3d(1, 0, 0, " ++ lerp 45 0 ++ "deg)"

        y =
            "rotate3d(0, 1, 0, " ++ lerp 15 0 ++ "deg)"

        z =
            "rotate3d(0, 0, 1, -" ++ lerp 35 0 ++ "deg)"

        scale =
            "scale(" ++ lerp 1.5 1 ++ ")"

        translate =
            "translate3d(" ++ "200" ++ "px, " ++ "200" ++ "px, " ++ "300" ++ "px)"
    in
    htmlAttribute <|
        Html.Attributes.style "transform" (x ++ " " ++ y ++ " " ++ z ++ " " ++ scale ++ " " ++ translate)


perspective =
    htmlAttribute <|
        Html.Attributes.style "perspective" "400px"


linearInterpolation : Float -> Float -> Float -> Float
linearInterpolation start end percent =
    start + (end - start) * percent


container model =
    Html.div
        [ style "width" "200vw"
        , style "height" "200vh"
        , style "background" "blue"
        , style "margin-left" "-50vw"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "perspective" "10000px"
        , style "transition" "transform 2s"
        ]
        [ layout model
        ]


layout : Model -> Element Msg
layout model =
    column
        [ width fill
        , height fill
        , rotate (clamp 0 400 model.scrollPosition / 400)
        , htmlAttribute <| Html.Attributes.style "transition" "transform 0.05s"
        , clip
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
