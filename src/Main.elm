module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (Attr, Attribute, Color, Device, DeviceClass(..), Element, alignBottom, alignRight, alignTop, centerX, centerY, classifyDevice, clipX, clipY, column, el, fill, height, htmlAttribute, image, link, maximum, mouseOver, moveDown, moveLeft, moveRight, moveUp, newTabLink, onLeft, padding, paddingEach, paddingXY, paragraph, px, row, spacing, text, textColumn, width)
import Element.Background as Background
import Element.Border exposing (shadow)
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes


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


globalPadding : Device -> Int
globalPadding device =
    case device.class of
        Phone ->
            scaledInt 4

        Tablet ->
            scaledInt 4

        Desktop ->
            scaledInt 8

        BigDesktop ->
            scaledInt 8


view : Model -> Html msg
view model =
    let
        device =
            classifyDevice model.window
    in
    Element.layout
        [ Font.family [ Font.typeface "futura-pt", Font.typeface "sans-serif" ]
        , width fill
        , clipX
        , clipY
        ]
        (column [ width fill ]
            [ column
                [ width (maximum 1600 fill)
                , centerX
                , spacing (scaledInt 6)
                , padding (globalPadding device)
                , clipX
                ]
                [ header
                , hero device
                , portfolio device
                ]
            , footer device
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
            [ button { text = "portfolio", url = "#portfolio" }
            , button { text = "contact", url = "#contact" }
            ]
        , splash device
        ]


button : { text : String, url : String } -> Element msg
button data =
    link
        [ mouseOver [ Font.color orange ]
        ]
        { label = smallCaps data.text, url = data.url }


smallCaps : String -> Element msg
smallCaps value =
    el [ Font.bold, Font.size (scaledInt 2) ] <| text (String.toUpper value)


titleFont : Device -> Attribute msg
titleFont device =
    let
        fontSize =
            case device.class of
                Phone ->
                    scaledInt 6

                Tablet ->
                    scaledInt 8

                Desktop ->
                    scaledInt 9

                BigDesktop ->
                    scaledInt 10
    in
    Font.size fontSize


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
                    scaled 4

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
        , htmlAttribute <| Html.Attributes.style "filter" "brightness(120%) saturate(70%)"
        , onLeft
            (column
                [ Font.bold
                , moveRight textShift
                , moveUp (scaled 9)
                , titleFont device
                , centerY
                ]
                [ el [ alignRight ] <| text "development"
                , el [ alignRight ] <| text "+ design"
                ]
            )
        ]
        { src = "splash.jpg", description = "" }


portfolio : Device -> Element msg
portfolio device =
    column
        [ htmlAttribute <| Html.Attributes.id "portfolio"
        , Element.behindContent
            (el
                [ height fill
                , width
                    (case device.class of
                        Phone ->
                            px 200

                        _ ->
                            px 300
                    )
                , Background.color lightGrey
                , Element.onRight
                    (el
                        [ Font.bold
                        , titleFont device
                        , alignRight
                        , moveLeft
                            (case device.class of
                                Phone ->
                                    scaled 11

                                _ ->
                                    scaled 8
                            )
                        , moveDown (scaled 10)
                        ]
                        (text "portfolio")
                    )
                ]
                Element.none
            )
        , Element.paddingEach
            { top =
                case device.class of
                    Phone ->
                        scaledInt 13

                    BigDesktop ->
                        scaledInt 15

                    _ ->
                        scaledInt 14
            , right = 0
            , bottom = scaledInt 10
            , left =
                case device.class of
                    Phone ->
                        scaledInt 4

                    _ ->
                        scaledInt 10
            }
        , spacing (scaledInt 11)
        ]
        [ portfolioSection device
            { title = "Digital Transformations"
            , subtitle = "design + development"
            , description = "A static website for a technology consulting firm. Uses the latest technology: Gatsby, Web Workers, and Emotion to deliver blazing-fast load times."
            , image =
                { src = "digital_transformations.png"
                , description = "A screen shot of the digital transformations website."
                , url = "https://digitaltransformations.us"
                }
            }
        , portfolioSection device
            { title = "Cogswell"
            , subtitle = "design + development"
            , description = "An event processing platform for Internet of Things devices. A single page app build using React."
            , image =
                { src = "cogswell.png"
                , description = "A screen shot of the cosgwell website."
                , url = "https://cogswell.io"
                }
            }
        ]


grey =
    Element.rgb255 100 100 100


lightGrey =
    Element.rgb255 220 220 220


orange =
    Element.rgb255 206 73 0


portfolioSection : Device -> { title : String, subtitle : String, description : String, image : { src : String, description : String, url : String } } -> Element msg
portfolioSection device data =
    (case device.class of
        Phone ->
            column

        Tablet ->
            column

        _ ->
            row
    )
        [ spacing (scaledInt 4) ]
        [ column [ spacing 6, width (maximum 450 fill), alignTop ]
            [ paragraph [] [ smallCaps data.title ]
            , paragraph [ Font.color grey ] [ smallCaps data.subtitle ]
            , paragraph [] [ text data.description ]
            ]
        , newTabLink [ width fill ]
            { url = data.image.url
            , label =
                image
                    [ alignTop
                    , width fill
                    , shadow { offset = ( 0, 10 ), size = 0, blur = 30, color = Element.rgba255 0 0 0 0.2 }
                    , mouseOver [ moveUp 12 ]
                    , htmlAttribute <| Html.Attributes.style "transition" "transform 0.3s"
                    ]
                    { src = data.image.src, description = data.image.description }
            }
        ]


footer : Device -> Element msg
footer device =
    el
        [ htmlAttribute <| Html.Attributes.id "contact"
        , Element.paddingEach
            { top = 0
            , right = globalPadding device
            , bottom = globalPadding device
            , left = 0
            }
        , width fill
        , Element.behindContent
            (el
                [ width fill
                , height fill
                , Background.color orange
                , moveDown (toFloat <| globalPadding device)
                ]
                Element.none
            )
        ]
        (el
            [ width fill
            , Background.color (Element.rgb255 26 26 26)
            , Font.color (Element.rgb255 255 255 255)
            , paddingXY (globalPadding device) (scaledInt 8)
            ]
            (column [ spacing (scaledInt 5), width (maximum 1366 fill), centerX ]
                [ row [ width fill ]
                    [ el [ Font.bold, titleFont device ] <| text "about"
                    , image [ width (px 80), alignRight ] { src = "small_logo.svg", description = "" }
                    ]
                , footerSection { title = "contact", body = text "adrian.aleixandre@gmail.com" }
                , footerSection
                    { title = "bio"
                    , body =
                        textColumn [ spacing (scaledInt 1), width fill ]
                            [ paragraph []
                                [ text "I am a passionate web developer with an eye for design. I have three years experience building web applications, solving complex design and technical problems, and educating my coworkers on the latest trends in both pixels and bits."
                                ]
                            , paragraph []
                                [ text "I blend the creative with the technical to craft products worth loving."
                                ]
                            ]
                    }
                , footerSection { title = "this site", body = paragraph [] [ text "Designed and built by me in Elm." ] }
                ]
            )
        )


footerSection : { title : String, body : Element msg } -> Element msg
footerSection { title, body } =
    column [ spacing (scaledInt 1), width (maximum 450 fill) ]
        [ smallCaps title
        , body
        ]


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
