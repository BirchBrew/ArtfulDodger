module Main exposing (..)

import Html exposing (Html, br, button, div, form, h2, hr, input, li, p, table, tbody, td, text, tr, ul)
import Html.Attributes exposing (placeholder, style, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode
import Json.Encode
import Phoenix.Channel
import Phoenix.Push
import Phoenix.Socket
import Platform.Cmd


-- Constants


welcomeTopic : String
welcomeTopic =
    "welcome"



-- MAIN


type alias Flags =
    { socketServer : String
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
    }


type alias Topic =
    String


type Screen
    = Welcome
    | Lobby
    | Game


type alias NameTag =
    String


initModelCmd : String -> ( Model, Cmd Msg )
initModelCmd socketServer =
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
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    initModelCmd flags.socketServer


initPhxSocket : String -> Phoenix.Socket.Socket Msg
initPhxSocket socketServer =
    Phoenix.Socket.init socketServer
        -- TODO remove this `withDebug` before going live
        |> Phoenix.Socket.withDebug



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Phoenix.Socket.listen model.phxSocket PhoenixMsg



-- COMMANDS
-- PHOENIX STUFF


tableDecoder : Json.Decode.Decoder Topic
tableDecoder =
    Json.Decode.field "table" Json.Decode.string


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
            case Json.Decode.decodeValue tableDecoder raw of
                Ok table ->
                    let
                        tableTopic =
                            "table:" ++ table

                        newModel =
                            { model | tableTopic = Just tableTopic, currentScreen = Lobby }

                        ( newLeaveModel, leaveCmd ) =
                            update LeaveWelcomeChannel newModel

                        ( newJoinModel, joinCmd ) =
                            update (JoinChannel <| tableTopic) newLeaveModel
                    in
                    ( newJoinModel, Cmd.batch [ leaveCmd, joinCmd ] )

                Err error ->
                    ( model, Cmd.none )

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
            div []
                [ h2 [] [ text "Game:" ]
                , playersListView model
                , button [ onClick ChooseCategory ] [ text "Choose Topic" ]
                , button [ onClick ProgressGame ] [ text "Progress Game" ]
                , div
                    []
                    [ text "That's all, folks!" ]
                ]


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
