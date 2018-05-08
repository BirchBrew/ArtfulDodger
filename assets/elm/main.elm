module Main exposing (..)

import Html exposing (Html, br, button, div, form, h2, hr, input, li, p, table, tbody, td, text, tr, ul)
import Html.Attributes exposing (attribute, class, id, placeholder, style, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode
import Json.Encode
import Mouse exposing (onContextMenu)
import Phoenix.Channel
import Phoenix.Push
import Phoenix.Socket
import Platform.Cmd
import Pointer
import Svg exposing (Svg, polyline, svg)
import Svg.Attributes exposing (class, fill, points, preserveAspectRatio, stroke, strokeWidth, viewBox)
import Window


-- Constants


welcomeTopic : String
welcomeTopic =
    "welcome"


drawingWindowRatio : Int
drawingWindowRatio =
    50



-- MAIN


type alias Flags =
    { socketServer : String
    , windowWidth : Int
    , windowHeight : Int
    }


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type Msg
    = PhoenixMsg (Phoenix.Socket.Msg Msg)
    | JoinChannel String
    | LeaveWelcomeChannel
    | ChangeScreen Screen
    | RequestNewTable
    | RequestJoinTable String
    | JoinTable Json.Encode.Value
    | JoinTableError Json.Encode.Value
    | Table String
    | NameTagChange NameTag
    | UpdateState Json.Encode.Value
    | PushStartGame
    | UpdateGame Json.Encode.Value
    | ProgressGame
    | ChooseCategory
    | StartGame Json.Encode.Value
    | Down Pointer.Event
    | Move Pointer.Event
    | Up Pointer.Event
    | Resize Int Int
    | None


type alias Model =
    { messages : List String
    , phxSocket : Phoenix.Socket.Socket Msg
    , currentScreen : Screen
    , tableTopic : Maybe Topic
    , tableRequest : String
    , errorText : String
    , nameTag : NameTag
    , nameTags : List NameTag
    , players : List Player
    , player_id : Int
    , mouseDown : Bool
    , lines : List Line
    , currentLine : Line
    , offCanvas : Bool
    , windowHeight : Int
    , windowWidth : Int
    }


type alias Topic =
    String


type Screen
    = Welcome
    | Lobby
    | Game


type alias NameTag =
    String


type alias Point =
    String


type alias Line =
    { color : String
    , points : List Point
    }


initModelCmd : Int -> Int -> String -> ( Model, Cmd Msg )
initModelCmd windowWidth windowHeight socketServer =
    update
        (JoinChannel welcomeTopic)
        { messages = []
        , phxSocket = initPhxSocket socketServer
        , currentScreen = Welcome
        , tableTopic = Nothing
        , errorText = ""
        , tableRequest = ""
        , nameTag = ""
        , nameTags = []
        , players = []
        , player_id = -1
        , mouseDown = False
        , currentLine = Line "black" []
        , lines = []
        , offCanvas = False
        , windowHeight = windowHeight
        , windowWidth = windowWidth
        }


init : Flags -> ( Model, Cmd Msg )
init { windowWidth, windowHeight, socketServer } =
    initModelCmd windowWidth windowHeight socketServer


initPhxSocket : String -> Phoenix.Socket.Socket Msg
initPhxSocket socketServer =
    Phoenix.Socket.init socketServer
        -- TODO remove this `withDebug` before going live
        |> Phoenix.Socket.withDebug



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Phoenix.Socket.listen model.phxSocket PhoenixMsg
        , Window.resizes (\{ height, width } -> Resize height width)
        ]



-- COMMANDS
-- PHOENIX STUFF


type alias TableState =
    { table : String
    , player_id : Int
    }


tableStateDecoder : Json.Decode.Decoder TableState
tableStateDecoder =
    Json.Decode.map2 TableState
        (Json.Decode.field "table" Json.Decode.string)
        (Json.Decode.field "id" Json.Decode.int)


errorDecoder : Json.Decode.Decoder String
errorDecoder =
    Json.Decode.field "error" Json.Decode.string


namesDecoder : Json.Decode.Decoder (List String)
namesDecoder =
    Json.Decode.field "names" (Json.Decode.list Json.Decode.string)


type alias Player =
    { player_id : Int
    , name : String
    , isActive : Bool
    , seat : Int
    }


playerDecoder : Json.Decode.Decoder Player
playerDecoder =
    Json.Decode.map4 Player
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "is_active" Json.Decode.bool)
        (Json.Decode.field "seat" Json.Decode.int)


type alias GameState =
    { players : List Player
    }


gameDecoder : Json.Decode.Decoder GameState
gameDecoder =
    Json.Decode.map GameState
        (Json.Decode.field "players" (Json.Decode.list playerDecoder))



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Needed by our library. Do not change this clause!
        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
            )

        -- All Custom Messages:
        None ->
            ( model, Cmd.none )

        Table name ->
            ( { model | tableRequest = name }, Cmd.none )

        NameTagChange nameTag ->
            let
                payload =
                    Json.Encode.object [ ( "name", Json.Encode.string nameTag ) ]

                push =
                    Phoenix.Push.init "name_tag" (Maybe.withDefault "" model.tableTopic)
                        |> Phoenix.Push.withPayload payload
                        |> Phoenix.Push.onOk JoinTable

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push push model.phxSocket
            in
            ( { model
                | phxSocket = phxSocket
                , nameTag = nameTag
              }
            , Cmd.map PhoenixMsg phxCmd
            )

        RequestNewTable ->
            let
                push =
                    Phoenix.Push.init "new_table" welcomeTopic
                        |> Phoenix.Push.onOk JoinTable

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push push model.phxSocket
            in
            ( { model
                | phxSocket = phxSocket
              }
            , Cmd.map PhoenixMsg phxCmd
            )

        RequestJoinTable name ->
            let
                payload =
                    Json.Encode.object [ ( "table", Json.Encode.string name ) ]

                push =
                    Phoenix.Push.init "join_table" welcomeTopic
                        |> Phoenix.Push.withPayload payload
                        |> Phoenix.Push.onOk JoinTable
                        |> Phoenix.Push.onError JoinTableError

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push push model.phxSocket
            in
            ( { model
                | phxSocket = phxSocket
              }
            , Cmd.map PhoenixMsg phxCmd
            )

        JoinTable raw ->
            case Json.Decode.decodeValue tableStateDecoder raw of
                Ok tableState ->
                    let
                        tableTopic =
                            "table:" ++ tableState.table

                        newModel =
                            { model | tableTopic = Just tableTopic, currentScreen = Lobby, player_id = tableState.player_id }

                        ( newLeaveModel, leaveCmd ) =
                            update LeaveWelcomeChannel newModel

                        ( newJoinModel, joinCmd ) =
                            update (JoinChannel <| tableTopic) newLeaveModel
                    in
                    ( newJoinModel, Cmd.batch [ leaveCmd, joinCmd ] )

                Err error ->
                    ( { model | errorText = "failed to join table" }, Cmd.none )

        JoinTableError raw ->
            case Json.Decode.decodeValue errorDecoder raw of
                Ok errorMsg ->
                    ( { model | errorText = errorMsg }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

        JoinChannel topic ->
            let
                channel =
                    Phoenix.Channel.init topic

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.join channel model.phxSocket

                phxSocket_ =
                    Phoenix.Socket.on "update" topic UpdateState phxSocket
                        |> Phoenix.Socket.on "update_game" (Maybe.withDefault "" model.tableTopic) UpdateGame
                        |> Phoenix.Socket.on "start_game" (Maybe.withDefault "" model.tableTopic) StartGame
            in
            ( { model | phxSocket = phxSocket_ }
            , Cmd.map PhoenixMsg phxCmd
            )

        LeaveWelcomeChannel ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.leave welcomeTopic model.phxSocket
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
            )

        PushStartGame ->
            let
                push =
                    Phoenix.Push.init "start_game" (Maybe.withDefault "" model.tableTopic)

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push push model.phxSocket
            in
            ( { model
                | phxSocket = phxSocket
              }
            , Cmd.map PhoenixMsg phxCmd
            )

        StartGame _ ->
            ( { model | currentScreen = Game }, Cmd.none )

        UpdateGame raw ->
            case Json.Decode.decodeValue gameDecoder raw of
                Ok gameState ->
                    ( { model | players = gameState.players }, Cmd.none )

                Err error ->
                    ( { model | errorText = "failed to update game" }, Cmd.none )

        ChangeScreen screen ->
            ( { model | currentScreen = screen }, Cmd.none )

        UpdateState raw ->
            case Json.Decode.decodeValue namesDecoder raw of
                Ok nameTags ->
                    ( { model | nameTags = nameTags }, Cmd.none )

                Err error ->
                    ( { model | errorText = "couldn't update state" }, Cmd.none )

        ProgressGame ->
            let
                push =
                    Phoenix.Push.init "progress_game" (Maybe.withDefault "" model.tableTopic)

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push push model.phxSocket
            in
            ( { model
                | phxSocket = phxSocket
              }
            , Cmd.map PhoenixMsg phxCmd
            )

        ChooseCategory ->
            let
                push =
                    Phoenix.Push.init "choose_category" (Maybe.withDefault "" model.tableTopic)

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push push model.phxSocket
            in
            ( { model
                | phxSocket = phxSocket
              }
            , Cmd.map PhoenixMsg phxCmd
            )

        Down event ->
            ( { model | mouseDown = True }, Cmd.none )

        Move event ->
            handleMouseMove model event

        Up event ->
            handleMouseUp model

        Resize h w ->
            ( { model | windowHeight = h, windowWidth = w }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.currentScreen of
        Welcome ->
            div []
                [ h2 [] [ text "A Phony Painter goes to NJ" ]
                , p [] [ text "How may I serve you today?" ]
                , button [ onClick RequestNewTable ] [ text "I want a new table" ]
                , hr [] []
                , input [ type_ "text", placeholder "enter table name", onInput Table ] []
                , button [ onClick <| RequestJoinTable model.tableRequest ] [ text "I'm meeting my friends" ]
                , p [ style errStyle ] [ text model.errorText ]
                ]

        Lobby ->
            div []
                [ h2 [] [ text <| Maybe.withDefault "" model.tableTopic ]

                -- TODO replace with drawn NameTag
                , input [ type_ "text", placeholder "enter NameTag", onInput NameTagChange ] []
                , nameTagView model
                , button [ onClick PushStartGame ] [ text "go to Game" ]
                ]

        Game ->
            div
                [ style
                    [ ( "height", "100%" )
                    ]
                ]
                [ div
                    [ style
                        [ ( "height", toString (100 - drawingWindowRatio) ++ "%" )
                        ]
                    ]
                    [ h2 [] [ text "Game:" ]
                    , playersListView model
                    , viewDrawing model
                    , button [ onClick ChooseCategory ] [ text "Choose Topic" ]
                    , button [ onClick ProgressGame ] [ text "Progress Game" ]
                    , nameTagView model
                    ]
                , drawingSpace model
                ]


getActivePlayerHelper : Maybe Player -> Player
getActivePlayerHelper player =
    case player of
        Just elem ->
            elem

        Nothing ->
            Debug.crash "Player list didn't have you in it!"


choicesView : Model -> Html Msg
choicesView model =
    let
        activePlayer =
            getActivePlayerHelper (List.filter (\player -> player.player_id == model.player_id) model.players |> List.head)
    in
    if activePlayer.seat == 0 && activePlayer.isActive == True then
        button [ onClick ChooseCategory ] [ text "Choose Topic" ]
    else if activePlayer.isActive == True then
        button [ onClick ProgressGame ] [ text "Progress Game" ]
    else
        text ""


viewDrawing : Model -> Html Msg
viewDrawing model =
    svg
        [ getViewBox model
        , preserveAspectRatio "none"
        , Svg.Attributes.width "100px"
        , Svg.Attributes.height "50px"
        ]
        (drawLines model)


viewBoxWidth : Float
viewBoxWidth =
    1920


viewBoxHeight : Float
viewBoxHeight =
    1080


getViewBox : Model -> Html.Attribute msg
getViewBox model =
    viewBox <| "0 0 " ++ toString viewBoxWidth ++ " " ++ toString viewBoxHeight


drawingSpace : Model -> Html Msg
drawingSpace model =
    svg (getDrawingSpaceAttributes model) (drawLines model)


getDrawingSpaceAttributes : Model -> List (Html.Attribute Msg)
getDrawingSpaceAttributes model =
    [ style
        [ ( "height", toString drawingWindowRatio ++ "%" )
        , ( "width", "100%" )
        ]
    , getViewBox model
    , preserveAspectRatio "none"

    -- pointer capture hack to continue "globally" the event anywhere on document.
    , attribute "onpointerdown" "event.target.setPointerCapture(event.pointerId);"
    , onContextMenu disableContextMenu
    ]
        ++ maybeListenForMove model


maybeListenForMove : Model -> List (Html.Attribute Msg)
maybeListenForMove { mouseDown } =
    let
        defaultList =
            [ Pointer.onDown Down
            , Pointer.onUp Up
            ]
    in
    case mouseDown of
        True ->
            Pointer.onMove Move :: defaultList

        False ->
            defaultList


handleMouseUp : Model -> ( Model, Cmd Msg )
handleMouseUp model =
    case List.length model.currentLine.points of
        -- nothing drawn, keep currentLine empty
        0 ->
            ( { model | mouseDown = False }, Cmd.none )

        -- something was drawn, so save currentLine and start new one
        _ ->
            let
                newLines =
                    model.currentLine :: model.lines
            in
            ( { model | mouseDown = False, lines = newLines, currentLine = Line "black" [] }, Cmd.none )


handleMouseMove : Model -> Pointer.Event -> ( Model, Cmd Msg )
handleMouseMove model event =
    case model.mouseDown of
        True ->
            case model.offCanvas of
                True ->
                    handleMouseUp { model | offCanvas = False }

                False ->
                    let
                        deadZone =
                            3

                        ( x, y ) =
                            event.pointer.offsetPos

                        currentPos =
                            relativePos model event

                        currentLine =
                            model.currentLine

                        points =
                            translatePos currentPos :: currentLine.points

                        newCurrentLine =
                            { currentLine | points = points }
                    in
                    if x < 0 || x >= toFloat model.windowWidth - deadZone || y < 0 || y >= toFloat (model.windowHeight * drawingWindowRatio // 100) - deadZone then
                        ( { model | currentLine = newCurrentLine, offCanvas = True }, Cmd.none )
                    else
                        ( { model | currentLine = newCurrentLine }, Cmd.none )

        False ->
            ( model, Cmd.none )


translatePos : ( Float, Float ) -> String
translatePos ( x, y ) =
    toString x ++ "," ++ toString y


relativePos : Model -> Pointer.Event -> ( Float, Float )
relativePos model pointerEvent =
    let
        ( x, y ) =
            pointerEvent.pointer.offsetPos

        normalX =
            x / toFloat model.windowWidth * viewBoxWidth

        normalY =
            y / toFloat (model.windowHeight * drawingWindowRatio // 100) * viewBoxHeight
    in
    ( normalX, normalY )


drawLines : Model -> List (Svg msg)
drawLines { currentLine, lines } =
    List.map (\line -> polyline [ points (pointString line.points), stroke line.color, strokeWidth "1em", fill "none" ] []) (currentLine :: lines)


pointString : List Point -> String
pointString points =
    String.join " " points


disableContextMenu : a -> Msg
disableContextMenu event =
    None


nameTagView : Model -> Html msg
nameTagView model =
    div []
        [ h2 [] [ text "Painters" ]
        , ul [] <| displayNameTags model.nameTags
        ]


playersListView : Model -> Html msg
playersListView model =
    div []
        [ h2 [] [ text "Painters" ]
        , ul [] <| displayPlayer model.players
        ]


displayPlayer : List Player -> List (Html.Html msg)
displayPlayer players =
    List.map
        (\player ->
            li []
                [ text ("Name: " ++ player.name)
                , ul
                    []
                    [ li [] [ text ("Player Id: " ++ toString player.player_id) ]
                    , li [] [ text ("Seat: " ++ toString player.seat) ]
                    , li [] [ text ("Active Player? " ++ toString player.isActive) ]
                    ]
                ]
        )
        players


displayNameTags : List NameTag -> List (Html.Html msg)
displayNameTags nameTags =
    List.map (\nameTag -> li [] [ text nameTag ]) nameTags


errStyle : List ( String, String )
errStyle =
    [ ( "color", "red" ) ]
