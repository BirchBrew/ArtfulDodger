module Main exposing (..)

import Bulma.CDN exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Dict
import Html exposing (Html, br, div, form, h2, hr, input, li, p, table, tbody, td, text, tr, ul)
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
import String
import Svg exposing (Svg, polyline, svg)
import Svg.Attributes exposing (class, fill, points, preserveAspectRatio, stroke, strokeWidth, viewBox)
import Window


-- Constants


welcomeTopic : String
welcomeTopic =
    "welcome"


drawingWindowRatio : Int
drawingWindowRatio =
    80


enterKeyCode : Int
enterKeyCode =
    13



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
    | ChooseCategory
    | Down Pointer.Event
    | Move Pointer.Event
    | Up Pointer.Event
    | KeyDown Int
    | Resize Int Int
    | VoteFor String
    | None


type alias Topic =
    String


type BigState
    = Welcome
    | Lobby
    | Game
    | End


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
    List Point


type alias Model =
    { phxSocket : Phoenix.Socket.Socket Msg
    , state : TableState
    , playerId : String
    , tableTopic : Maybe Topic
    , tableRequest : Maybe String
    , errorText : String
    , mouseDown : Bool
    , currentLine : Line
    , offCanvas : Bool
    , drawingSpaceEdgePx : Float
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
        , currentLine = []
        , offCanvas = False
        , drawingSpaceEdgePx =
            calculateDrawingSpaceEdgePx windowWidth windowHeight
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

                    "end" ->
                        Json.Decode.succeed End

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
    { seat : Int
    , name : String
    , role : Role
    , color : String
    , nameTagLines : List Line
    , paintLines : List Line
    , votedFor : Maybe String
    }


playerDecoder : Json.Decode.Decoder Player
playerDecoder =
    Json.Decode.succeed Player
        |> Json.Decode.Extra.andMap (Json.Decode.field "seat" Json.Decode.int)
        |> Json.Decode.Extra.andMap (Json.Decode.field "name" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "role" roleDecoder)
        |> Json.Decode.Extra.andMap (Json.Decode.field "color" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "name_tag_lines" <| Json.Decode.list lineDecoder)
        |> Json.Decode.Extra.andMap (Json.Decode.field "paint_lines" <| Json.Decode.list lineDecoder)
        |> Json.Decode.Extra.andMap (Json.Decode.field "voted_for" (Json.Decode.maybe Json.Decode.string))


lineDecoder : Json.Decode.Decoder Line
lineDecoder =
    Json.Decode.list Json.Decode.string


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
            ( { model | tableRequest = Just <| transformInput name }, Cmd.none )

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

        VoteFor playerId ->
            let
                payload =
                    Json.Encode.object
                        [ ( "for", Json.Encode.string playerId )
                        ]

                push =
                    Phoenix.Push.init "vote_for" (Maybe.withDefault "" model.tableTopic)
                        |> Phoenix.Push.withPayload payload

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

        KeyDown key ->
            if key == enterKeyCode then
                update (RequestJoinTable (Maybe.withDefault "" model.tableRequest)) model
            else
                ( model, Cmd.none )

        Resize h w ->
            ( { model | drawingSpaceEdgePx = calculateDrawingSpaceEdgePx h w }, Cmd.none )


transformInput : String -> String
transformInput input =
    input |> String.toUpper |> String.left 4



-- VIEW


view : Model -> Html Msg
view model =
    Html.main_
        [ style
            [ ( "height", "100%" )
            , ( "touch-action", "none" )
            ]
        ]
        [ stylesheet
        , viewRest model
        ]


viewWelcome : Model -> Html Msg
viewWelcome model =
    hero { heroModifiers | size = Large, color = Dark }
        []
        [ heroBody []
            [ container []
                [ title H1 [] [ text "A Dodgy Artist" ]
                , subtitle H1 [] [ text "Goes to NJ" ]
                , br [] []
                , columns columnsModifiers
                    []
                    [ column columnModifiers
                        []
                        [ newTableButton
                        ]
                    , column columnModifiers
                        []
                        [ connectedFields Left
                            []
                            [ tableInput model
                            , joinTableButton model
                            ]
                        , controlHelp Danger [] [ text model.errorText ]
                        ]
                    ]
                ]
            ]
        ]


myButtonModifiers : ButtonModifiers msg
myButtonModifiers =
    { buttonModifiers | rounded = False, color = Info }


newTableButton : Html Msg
newTableButton =
    button myButtonModifiers [ fullWidth, onClick RequestNewTable ] [ text "New Table" ]


tableInput : Model -> Html Msg
tableInput model =
    controlInput { controlInputModifiers | rounded = False, expanded = True }
        []
        [ type_ "text"
        , placeholder "enter table name"
        , onInput Table
        , Html.Attributes.value (Maybe.withDefault "" model.tableRequest)
        , onKeyDown KeyDown
        , Html.Attributes.autofocus True
        ]
        []


joinTableButton : Model -> Html Msg
joinTableButton model =
    button myButtonModifiers [ onClick <| RequestJoinTable (Maybe.withDefault "" model.tableRequest) ] [ text "Join Table" ]


onKeyDown : (Int -> Msg) -> Html.Attribute Msg
onKeyDown msgTag =
    Html.Events.on "keydown" (Json.Decode.map msgTag Html.Events.keyCode)


viewLobby : Model -> Html Msg
viewLobby model =
    hero { heroModifiers | size = Large, color = Dark }
        []
        [ heroHead []
            [ container []
                [ title H3 [] [ text <| Maybe.withDefault "" model.tableTopic ]
                ]
            ]
        , heroBody []
            [ container []
                [ nameTagView model
                , section Spaced
                    []
                    [ controlInput controlInputModifiers
                        []
                        [ type_ "text"
                        , placeholder "enter NameTag"
                        , onInput NameTagChange
                        , Html.Attributes.autofocus True
                        ]
                        []
                    , button myButtonModifiers [ onClick PushStartGame ] [ text "go to Game" ]
                    ]
                ]
            ]
        ]


viewGame : Model -> Html Msg
viewGame model =
    hero { heroModifiers | size = Large, color = Dark }
        []
        [ heroBody
            [ style [ ( "justify-content", "center" ) ]
            ]
            (case model.state.littleState of
                Pick ->
                    [ choicesView model
                    ]

                Draw ->
                    [ nameTagViewingSpace model
                    , fullDrawingSpace model
                    ]

                Vote ->
                    if isGameMaster model || hasVoted model == True then
                        []
                    else
                        [ votesView model ]
            )
        ]


viewRest : Model -> Html Msg
viewRest model =
    case model.state.bigState of
        Welcome ->
            viewWelcome model

        Lobby ->
            viewLobby model

        Game ->
            viewGame model

        End ->
            displayWinner model


displayWinner : Model -> Html msg
displayWinner model =
    text (Maybe.withDefault "" model.state.winner)


guaranteePlayerExists : Maybe Player -> Player
guaranteePlayerExists player =
    case player of
        Just elem ->
            elem

        Nothing ->
            Debug.crash "Player list didn't have you in it!"


guaranteeTupleExists : Maybe ( String, Player ) -> ( String, Player )
guaranteeTupleExists player =
    case player of
        Just elem ->
            elem

        Nothing ->
            Debug.crash "No game master in list!"


guaranteeStringExists : Maybe String -> String
guaranteeStringExists player =
    case player of
        Just elem ->
            elem

        Nothing ->
            Debug.crash "No active players!?"


getFirst : List String -> String
getFirst players =
    guaranteeStringExists (players |> List.head)


displayActivePlayers : Model -> List (Html Msg)
displayActivePlayers model =
    List.map
        (\player_id ->
            li []
                [ text player_id ]
        )
        model.state.activePlayers


isActivePlayer : Model -> Bool
isActivePlayer model =
    List.member model.playerId model.state.activePlayers


choicesView : Model -> Html Msg
choicesView model =
    if isActivePlayer model && model.state.littleState == Pick then
        button myButtonModifiers [ onClick ChooseCategory ] [ text "Choose Topic" ]
    else
        text ""


viewBoxLength : Float
viewBoxLength =
    1024


getViewBox : Model -> Html.Attribute msg
getViewBox model =
    viewBox <| "0 0 " ++ toString viewBoxLength ++ " " ++ toString viewBoxLength


fullDrawingSpace : Model -> Html Msg
fullDrawingSpace model =
    drawingSpaceWithRatio (getDrawingSpaceAttributes model) 1.0 model


nameTagViewingSpace : Model -> Html Msg
nameTagViewingSpace model =
    drawingSpaceWithRatio (getNameTagViewingSpaceAttributes model) 0.1 model


drawingSpaceWithRatio : List (Html.Attribute Msg) -> Float -> Model -> Html Msg
drawingSpaceWithRatio attributes ratio model =
    let
        pxStr =
            toString (model.drawingSpaceEdgePx * ratio) ++ "px"
    in
    box
        [ style
            [ ( "padding", "0px" )
            , ( "height", pxStr )
            , ( "width", pxStr )
            , ( "background-color", "#f5f5f5" )
            ]
        ]
        [ svg attributes (drawLines model)
        ]


getDrawingSpaceAttributes : Model -> List (Html.Attribute Msg)
getDrawingSpaceAttributes model =
    [ getViewBox model

    -- pointer capture hack to continue "globally" the event anywhere on document.
    , attribute "onpointerdown" "event.target.setPointerCapture(event.pointerId);"
    , onContextMenu disableContextMenu
    ]
        ++ maybeListenForMove model


getNameTagViewingSpaceAttributes : Model -> List (Html.Attribute Msg)
getNameTagViewingSpaceAttributes model =
    [ getViewBox model
    , onContextMenu disableContextMenu
    ]


calculateDrawingSpaceEdgePx : Int -> Int -> Float
calculateDrawingSpaceEdgePx windowWidth windowHeight =
    min windowWidth windowHeight
        * drawingWindowRatio
        // 100
        |> toFloat


maybeListenForMove : Model -> List (Html.Attribute Msg)
maybeListenForMove model =
    let
        defaultList =
            [ Pointer.onDown Down
            , Pointer.onUp Up
            ]
    in
    if isActivePlayer model && model.state.littleState == Draw then
        case model.mouseDown of
            True ->
                Pointer.onMove Move :: defaultList

            False ->
                defaultList
    else
        []


handleMouseUp : Model -> ( Model, Cmd Msg )
handleMouseUp model =
    case List.length model.currentLine of
        -- nothing drawn, keep currentLine empty
        0 ->
            ( { model | mouseDown = False }, Cmd.none )

        -- something was drawn, so save currentLine and start new one
        _ ->
            let
                push =
                    Phoenix.Push.init "progress_game" (Maybe.withDefault "" model.tableTopic)

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push push model.phxSocket
            in
            ( { model | mouseDown = False, currentLine = [], phxSocket = phxSocket }, Cmd.map PhoenixMsg phxCmd )


handleMouseMove : Model -> Pointer.Event -> ( Model, Cmd Msg )
handleMouseMove model event =
    case model.mouseDown of
        True ->
            case model.offCanvas of
                True ->
                    handleMouseUp { model | offCanvas = False }

                False ->
                    let
                        ( x, y ) =
                            event.pointer.offsetPos

                        currentPos =
                            relativePos model event

                        currentLine =
                            model.currentLine

                        newCurrentLine =
                            translatePos currentPos :: currentLine

                        payload =
                            Json.Encode.object
                                [ ( "line", Json.Encode.list <| List.map Json.Encode.string newCurrentLine )
                                ]

                        push =
                            Phoenix.Push.init "paint_line" (Maybe.withDefault "" model.tableTopic)
                                |> Phoenix.Push.withPayload payload

                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.push push model.phxSocket
                    in
                    if x < 0 || x >= model.drawingSpaceEdgePx || y < 0 || y >= model.drawingSpaceEdgePx then
                        ( { model | currentLine = newCurrentLine, offCanvas = True, phxSocket = phxSocket }, Cmd.map PhoenixMsg phxCmd )
                    else
                        ( { model | currentLine = newCurrentLine, phxSocket = phxSocket }, Cmd.map PhoenixMsg phxCmd )

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
            x / model.drawingSpaceEdgePx * viewBoxLength

        normalY =
            y / model.drawingSpaceEdgePx * viewBoxLength
    in
    ( normalX, normalY )


drawLines : Model -> List (Svg msg)
drawLines { state } =
    let
        sortedPlayers =
            state.players
                |> Dict.values
                |> List.sortBy .seat

        svgLines =
            List.map
                (\{ color, paintLines } ->
                    List.filterMap
                        (\line ->
                            case line of
                                [] ->
                                    Nothing

                                _ ->
                                    Just <| polyline [ points (pointString line), stroke color, strokeWidth "1em", fill "none" ] []
                        )
                        paintLines
                )
                sortedPlayers

        ( firstLines, secondLines ) =
            List.foldr svgLinesFolder ( [], [] ) svgLines
    in
    firstLines ++ secondLines


svgLinesFolder : List (Svg msg) -> ( List (Svg msg), List (Svg msg) ) -> ( List (Svg msg), List (Svg msg) )
svgLinesFolder lines ( f, s ) =
    case lines of
        [] ->
            ( f, s )

        firstLine :: [] ->
            ( firstLine :: f, s )

        secondLine :: firstLine :: [] ->
            ( firstLine :: f, secondLine :: s )

        _ ->
            Debug.crash "Like in Poker, you can't fold everything"


pointString : List Point -> String
pointString points =
    String.join " " points


disableContextMenu : a -> Msg
disableContextMenu event =
    None


nameTagView : Model -> Html msg
nameTagView model =
    section Spaced
        []
        (title H2 [] [ text "Painters" ]
            :: displayNameTags model.state.players
        )


votesView : Model -> Html Msg
votesView model =
    div []
        [ h2 [] [ text "Who is the Dodgy Artist?" ]
        , ul [] <| (playersExceptMeAndGameMaster model |> playerButtons)
        ]


playersExceptMeAndGameMaster : Model -> Dict.Dict String Player
playersExceptMeAndGameMaster model =
    Dict.remove model.playerId model.state.players |> removeGameMaster


removeGameMaster : Dict.Dict String Player -> Dict.Dict String Player
removeGameMaster players =
    let
        playerList =
            players |> Dict.toList

        gameMaster =
            List.filter (\player -> (player |> Tuple.second |> .role) == GameMaster) playerList |> List.head
    in
    Dict.remove (gameMaster |> guaranteeTupleExists |> Tuple.first) players


playerButtons : Dict.Dict String Player -> List (Html Msg)
playerButtons players =
    List.map
        (\( playerId, playerRecord ) ->
            button myButtonModifiers
                [ onClick (VoteFor playerId) ]
                [ text <| playerRecord.name
                ]
        )
        (Dict.toList
            players
        )


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
                    , li [] [ text ("Seat: " ++ toString player.seat) ]
                    ]
                ]
        )
        players


displayNameTags : Dict.Dict String Player -> List (Html.Html msg)
displayNameTags playerMap =
    List.map
        (\{ name } ->
            div []
                [ tag { tagModifiers | color = Warning } [] [ text name ]
                ]
        )
        (Dict.values playerMap)


isGameMaster : Model -> Bool
isGameMaster model =
    (Dict.get model.playerId model.state.players |> guaranteePlayerExists |> .role) == GameMaster


isTrickster : Model -> Bool
isTrickster model =
    (Dict.get model.playerId model.state.players |> guaranteePlayerExists |> .role) == Trickster


isBasicPlayer : Model -> Bool
isBasicPlayer model =
    (Dict.get model.playerId model.state.players |> guaranteePlayerExists |> .role) == BasicPlayer


hasVoted : Model -> Bool
hasVoted model =
    (Dict.get model.playerId model.state.players |> guaranteePlayerExists |> .votedFor) /= Nothing
