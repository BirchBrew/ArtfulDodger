module Model exposing (..)

import Dict
import Json.Encode
import Phoenix.Socket
import Pointer


type Msg
    = PhoenixMsg (Phoenix.Socket.Msg Msg)
    | JoinChannel String
    | LeaveWelcomeChannel
    | RequestNewTable
    | RequestJoinTable String
    | JoinTable Json.Encode.Value
    | JoinTableError Json.Encode.Value
    | Table String
    | UpdateState Json.Encode.Value
    | PushStartGame
    | ChooseSubject
    | Down Pointer.Event
    | Move Pointer.Event
    | MoveWithFreedom Pointer.Event
    | Up Pointer.Event
    | UpWithFreedom Pointer.Event
    | KeyDown Int
    | Resize Int Int
    | VoteFor String
    | GuessSubject
    | Validate Bool
    | ChooseName
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
    | Tricky
    | Check
    | WriteName


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


type alias TableState =
    { bigState : BigState
    , littleState : LittleState
    , subject : List Line
    , activePlayers : List String
    , winner : Maybe String
    , players : Dict.Dict String Player
    , tableName : String
    , remainingTurns : Int
    , connectedComputers : Int
    }


type alias Player =
    { seat : Int
    , name : List Line
    , role : Role
    , color : String
    , paintLines : List Line
    , votedFor : Maybe String
    }


type alias Model =
    { phxSocket : Phoenix.Socket.Socket Msg
    , state : TableState
    , playerId : String
    , tableTopic : Maybe Topic
    , tableRequest : Maybe String
    , errorText : String
    , mouseDown : Bool
    , currentLine : Line
    , currentSoloDrawing : List Line
    , offCanvas : Bool
    , drawingSpaceEdgePx : Float
    , hasEnteredName : Bool
    }
