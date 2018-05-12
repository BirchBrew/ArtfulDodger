module Main exposing (..)

import Dict
import Html exposing (Html, br, button, div, form, h2, hr, input, li, p, table, tbody, td, text, tr, ul)
import Html.Attributes exposing (attribute, class, id, placeholder, style, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode
import Json.Decode.Extra
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
    | RequestNewTable
    | RequestJoinTable String
    | JoinTable Json.Encode.Value
    | JoinTableError Json.Encode.Value
    | Table String
    | NameTagChange NameTag
    | UpdateState Json.Encode.Value
    | PushStartGame
    | ProgressGame
    | ChooseCategory
    | Down Pointer.Event
    | Move Pointer.Event
    | Up Pointer.Event
    | Resize Int Int
    | None


type alias Topic =
    String


type BigState
    = Welcome
    | Lobby
    | Game


type LittleState
    = Pick
    | Draw
    | Vote


type Role
    = GameMaster
    | Trickster
    | BasicPlayer


type alias NameTag =
    String


type alias Point =
    String


type alias Line =
    { color : String
    , points : List Point
    }


type alias Model =
    { phxSocket : Phoenix.Socket.Socket Msg
    , state : TableState
    , playerId : String
    , tableTopic : Maybe Topic
    , tableRequest : Maybe String
    , errorText : String
    , mouseDown : Bool
    , lines : List Line
    , currentLine : Line
    , offCanvas : Bool
    , windowHeight : Int
    , windowWidth : Int
    }


initModelCmd : Int -> Int -> String -> ( Model, Cmd Msg )
initModelCmd windowWidth windowHeight socketServer =
    update
        (JoinChannel welcomeTopic)
        { phxSocket = initPhxSocket socketServer
        , tableRequest = Nothing
        , tableTopic = Nothing
        , errorText = ""
        , mouseDown = False
        , lines = []
        , currentLine = Line "black" []
        , offCanvas = False
        , windowHeight = windowHeight
        , windowWidth = windowWidth
        , state =
            { bigState = Welcome
            , littleState = Pick
            , topic = Nothing
            , category = Nothing
            , activePlayers = []
            , winner = Nothing
            , players = Dict.empty
            , tableName = ""
            , remainingTurns = 0
            , connectedComputers = 0
            }
        , playerId = ""
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
    { bigState : BigState
    , littleState : LittleState
    , topic : Maybe String
    , category : Maybe String
    , activePlayers : List String
    , winner : Maybe String
    , players : Dict.Dict String Player
    , tableName : String
    , remainingTurns : Int
    , connectedComputers : Int
    }


bigStateDecoder : Json.Decode.Decoder BigState
bigStateDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "welcome" ->
                        Json.Decode.succeed Welcome

                    "lobby" ->
                        Json.Decode.succeed Lobby

                    "game" ->
                        Json.Decode.succeed Game

                    _ ->
                        Debug.crash "Unknown big state"
            )


littleStateDecoder : Json.Decode.Decoder LittleState
littleStateDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "pick" ->
                        Json.Decode.succeed Pick

                    "draw" ->
                        Json.Decode.succeed Draw

                    "vote" ->
                        Json.Decode.succeed Vote

                    _ ->
                        Debug.crash "Unknown little state"
            )


roleDecoder : Json.Decode.Decoder Role
roleDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "game_master" ->
                        Json.Decode.succeed GameMaster

                    "trickster" ->
                        Json.Decode.succeed Trickster

                    "player" ->
                        Json.Decode.succeed BasicPlayer

                    _ ->
                        Debug.crash "Unknown role"
            )


tableStateDecoder : Json.Decode.Decoder TableState
tableStateDecoder =
    Json.Decode.succeed TableState
        |> Json.Decode.Extra.andMap (Json.Decode.field "big_state" bigStateDecoder)
        |> Json.Decode.Extra.andMap (Json.Decode.field "little_state" littleStateDecoder)
        |> Json.Decode.Extra.andMap (Json.Decode.field "topic" (Json.Decode.maybe Json.Decode.string))
        |> Json.Decode.Extra.andMap (Json.Decode.field "category" (Json.Decode.maybe Json.Decode.string))
        |> Json.Decode.Extra.andMap (Json.Decode.field "active_players" (Json.Decode.list Json.Decode.string))
        |> Json.Decode.Extra.andMap (Json.Decode.field "winner" (Json.Decode.maybe Json.Decode.string))
        |> Json.Decode.Extra.andMap (Json.Decode.field "players" (Json.Decode.dict playerDecoder))
        |> Json.Decode.Extra.andMap (Json.Decode.field "table_name" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "remaining_turns" Json.Decode.int)
        |> Json.Decode.Extra.andMap (Json.Decode.field "connected_computers" Json.Decode.int)


errorDecoder : Json.Decode.Decoder String
errorDecoder =
    Json.Decode.field "error" Json.Decode.string


namesDecoder : Json.Decode.Decoder (List String)
namesDecoder =
    Json.Decode.field "names" (Json.Decode.list Json.Decode.string)


type alias Player =
    { seat : Maybe Int
    , name : String
    , role : Role
    }


playerDecoder : Json.Decode.Decoder Player
playerDecoder =
    Json.Decode.map3 Player
        (Json.Decode.field "seat" (Json.Decode.maybe Json.Decode.int))
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "role" roleDecoder)


type alias GameState =
    { players : List Player
    }


gameDecoder : Json.Decode.Decoder GameState
gameDecoder =
    Json.Decode.map GameState
        (Json.Decode.field "players" (Json.Decode.list playerDecoder))


type alias JoinTableState =
    { table : String
    , playerId : String
    }


joinTableStateDecoder : Json.Decode.Decoder JoinTableState
joinTableStateDecoder =
    Json.Decode.map2 JoinTableState
        (Json.Decode.field "table" Json.Decode.string)
        (Json.Decode.field "id" Json.Decode.string)



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
            ( { model | tableRequest = Just name }, Cmd.none )

        NameTagChange nameTag ->
            let
                payload =
                    Json.Encode.object [ ( "name", Json.Encode.string nameTag ) ]

                push =
                    Phoenix.Push.init "name_tag" (Maybe.withDefault "" model.tableTopic)
                        |> Phoenix.Push.withPayload payload

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push push model.phxSocket
            in
            ( { model
                | phxSocket = phxSocket
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
            case Json.Decode.decodeValue joinTableStateDecoder raw of
                Ok joinTable ->
                    let
                        tableTopic =
                            "table:" ++ joinTable.table

                        st =
                            model.state

                        newState =
                            { st | bigState = Lobby }

                        newModel =
                            { model | tableTopic = Just tableTopic, playerId = joinTable.playerId, state = newState }

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

        UpdateState raw ->
            case Json.Decode.decodeValue tableStateDecoder raw of
                Ok tableState ->
                    ( { model | state = tableState }, Cmd.none )

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
    case model.state.bigState of
        Welcome ->
            div []
                [ h2 [] [ text "A Phony Painter goes to NJ" ]
                , p [] [ text "How may I serve you today?" ]
                , button [ onClick RequestNewTable ] [ text "I want a new table" ]
                , hr [] []
                , input [ type_ "text", placeholder "enter table name", onInput Table ] []
                , button [ onClick <| RequestJoinTable (Maybe.withDefault "" model.tableRequest) ] [ text "I'm meeting my friends" ]
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
                    , overviewView model
                    , playersListView model
                    , viewDrawing model
                    , choicesView model
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


getPlayerHelper : Maybe String -> String
getPlayerHelper player =
    case player of
        Just elem ->
            elem

        Nothing ->
            Debug.crash "No active players!?"


getFirst : List String -> String
getFirst players =
    getPlayerHelper (players |> List.head)


overviewView : Model -> Html Msg
overviewView model =
    div []
        [ h2 [] [ text "Active Players:" ]
        , ul [] <| displayActivePlayers model
        ]


displayActivePlayers : Model -> List (Html Msg)
displayActivePlayers model =
    List.map
        (\player_id ->
            li []
                [ text player_id ]
        )
        model.state.activePlayers


choicesView : Model -> Html Msg
choicesView model =
    let
        active_player_id =
            getFirst model.state.activePlayers

        active_player =
            Dict.get active_player_id model.state.players

        is_active =
            active_player_id == model.playerId
    in
    if is_active && model.state.littleState == Pick then
        button [ onClick ChooseCategory ] [ text "Choose Topic" ]
    else if is_active && model.state.littleState == Draw then
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
        , ul [] <| displayNameTags model.state.players
        ]


playersListView : Model -> Html msg
playersListView model =
    div []
        [ h2 [] [ text "Painters" ]
        , ul [] <| displayPlayer (Dict.values model.state.players)
        ]


displayPlayer : List Player -> List (Html.Html msg)
displayPlayer players =
    List.map
        (\player ->
            li []
                [ text ("Name: " ++ player.name)
                , ul
                    []
                    [ li [] [ text ("Role: " ++ toString player.role) ]
                    , li [] [ text ("Seat: " ++ toString (Maybe.withDefault -1 player.seat)) ]
                    ]
                ]
        )
        players


displayNameTags : Dict.Dict String Player -> List (Html.Html msg)
displayNameTags playerMap =
    List.map (\player -> li [] [ text player.name ]) (Dict.values playerMap)


errStyle : List ( String, String )
errStyle =
    [ ( "color", "red" ) ]
