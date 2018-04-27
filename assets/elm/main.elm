module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Mouse exposing (onContextMenu)
import Pointer
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


colorList : List String
colorList =
    [ "red", "orange", "yellow", "green", "blue", "violet" ]


firstColor : List String -> String
firstColor colorList =
    case List.head colorList of
        Just color ->
            color

        Nothing ->
            "black"


rotate : List a -> List a
rotate list =
    List.drop 1 list ++ List.take 1 list


type alias Model =
    { state : State
    , mouseDown : Bool
    , lines : List Line
    , currentLine : Line
    , colorList : List String
    }


type State
    = Idle
    | Drawing


type alias Point =
    String


type alias Line =
    { color : String
    , points : List Point
    }


type Msg
    = None
    | Down Pointer.Event
    | Move Pointer.Event
    | Up Pointer.Event


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        Down event ->
            ( { model | state = Drawing, mouseDown = True }, Cmd.none )

        Move event ->
            handleMouseMove model event

        Up event ->
            handleMouseUp model event


handleMouseUp : Model -> Pointer.Event -> ( Model, Cmd Msg )
handleMouseUp model event =
    case List.length model.currentLine.points of
        -- nothing drawn, keep currentLine empty
        0 ->
            ( { model | state = Idle, mouseDown = False }, Cmd.none )

        -- something was drawn, so save currentLine and start new one
        _ ->
            let
                newColors =
                    rotate model.colorList

                newLines =
                    model.currentLine :: model.lines
            in
            ( { model | state = Idle, mouseDown = False, lines = newLines, currentLine = Line (firstColor newColors) [], colorList = newColors }, Cmd.none )


handleMouseMove : Model -> Pointer.Event -> ( Model, Cmd Msg )
handleMouseMove model event =
    case model.mouseDown of
        True ->
            let
                currentLine =
                    model.currentLine

                points =
                    (translatePos <|
                        relativePos event
                    )
                        :: currentLine.points

                newCurrentLine =
                    { currentLine | points = points }
            in
            ( { model | state = Drawing, currentLine = newCurrentLine }, Cmd.none )

        False ->
            ( model, Cmd.none )


translatePos : ( Float, Float ) -> String
translatePos ( x, y ) =
    toString x ++ "," ++ toString y


view : Model -> Html Msg
view model =
    svg
        [ Pointer.onDown Down
        , Pointer.onMove Move
        , Pointer.onUp Up

        -- no touch-action (prevents scrolling and co.)
        , Html.Attributes.style [ ( "touch-action", "none" ) ]

        -- pointer capture hack to continue "globally" the event anywhere on document.
        , attribute "onpointerdown" "event.target.setPointerCapture(event.pointerId);"
        , onContextMenu disableContextMenu
        ]
        (drawLines
            model
        )


drawLines : Model -> List (Svg msg)
drawLines { currentLine, lines } =
    List.map (\line -> Svg.polyline [ points (pointString line.points), stroke line.color, strokeWidth "0.5em", fill "none" ] []) (currentLine :: lines)


pointString : List Point -> String
pointString points =
    String.join " " points


relativePos : Pointer.Event -> ( Float, Float )
relativePos pointerEvent =
    pointerEvent.pointer.offsetPos


disableContextMenu : a -> Msg
disableContextMenu event =
    None


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( Model Idle False [] (Line (firstColor colorList) []) colorList, Cmd.none )
