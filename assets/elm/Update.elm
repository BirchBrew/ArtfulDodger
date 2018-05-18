module Update exposing (update)

import Constant exposing (..)
import Decoder exposing (..)
import Json.Decode
import Json.Encode
import Model exposing (..)
import Phoenix.Channel
import Phoenix.Push
import Phoenix.Socket
import Pointer
import Utility exposing (..)


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
                payload =
                    Json.Encode.object
                        [ ( "name", Json.Encode.string model.name )
                        ]

                channel =
                    Phoenix.Channel.init topic |> Phoenix.Channel.withPayload payload

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

        GuessTopic ->
            let
                push =
                    Phoenix.Push.init "guess_topic" (Maybe.withDefault "" model.tableTopic)

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

        Validate isCorrect ->
            let
                payload =
                    Json.Encode.object
                        [ ( "is_correct", Json.Encode.bool isCorrect )
                        ]

                push =
                    Phoenix.Push.init "validate_guess" (Maybe.withDefault "" model.tableTopic)
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

        EnterNewTableScreen ->
            let
                oldState =
                    model.state

                newState =
                    { oldState | littleState = CreateTableScreen }
            in
            ( { model | state = newState }, Cmd.none )

        EnterJoinTableScreen ->
            let
                oldState =
                    model.state

                newState =
                    { oldState | littleState = JoinTableScreen }
            in
            ( { model | state = newState }, Cmd.none )

        NameChange name ->
            ( { model | name = name }, Cmd.none )


transformInput : String -> String
transformInput input =
    input |> String.toUpper |> String.left 4


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
