module Main exposing (..)

import Constant exposing (welcomeTopic)
import Dict
import Html exposing (Html, br, div, form, h2, hr, input, li, p, table, tbody, td, text, tr, ul)
import Model exposing (BigState(Welcome), LittleState(Pick), Model, Msg(..))
import Phoenix.Socket
import Platform.Cmd
import Update exposing (update)
import Utility exposing (calculateDrawingSpaceEdgePx)
import View exposing (view)
import Window


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


init : Flags -> ( Model, Cmd Msg )
init { windowWidth, windowHeight, socketServer } =
    initModelCmd windowWidth windowHeight socketServer


initPhxSocket : String -> Phoenix.Socket.Socket Msg
initPhxSocket socketServer =
    Phoenix.Socket.init socketServer



-- TODO remove this `withDebug` before going live
-- |> Phoenix.Socket.withDebug


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
        , currentSoloDrawing = []
        , offCanvas = False
        , drawingSpaceEdgePx =
            calculateDrawingSpaceEdgePx windowWidth windowHeight
        , state =
            { bigState = Welcome
            , littleState = Pick
            , subject = []
            , guess = []
            , activePlayers = []
            , winner = Nothing
            , players = Dict.empty
            , tableName = ""
            , remainingTurns = 0
            , connectedComputers = 0
            }
        , playerId = ""
        , hasEnteredName = False
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Phoenix.Socket.listen model.phxSocket PhoenixMsg
        , Window.resizes (\{ height, width } -> Resize height width)
        ]
