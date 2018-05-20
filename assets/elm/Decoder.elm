module Decoder exposing (..)

import Json.Decode
import Json.Decode.Extra
import Model exposing (..)


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

                    "tricky" ->
                        Json.Decode.succeed Tricky

                    "check" ->
                        Json.Decode.succeed Check

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
        |> Json.Decode.Extra.andMap (Json.Decode.field "subject" <| Json.Decode.list lineDecoder)
        |> Json.Decode.Extra.andMap (Json.Decode.field "guess" <| Json.Decode.list lineDecoder)
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


playerDecoder : Json.Decode.Decoder Player
playerDecoder =
    Json.Decode.succeed Player
        |> Json.Decode.Extra.andMap (Json.Decode.field "seat" Json.Decode.int)
        |> Json.Decode.Extra.andMap (Json.Decode.field "name" <| Json.Decode.list lineDecoder)
        |> Json.Decode.Extra.andMap (Json.Decode.field "role" roleDecoder)
        |> Json.Decode.Extra.andMap (Json.Decode.field "color" Json.Decode.string)
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
