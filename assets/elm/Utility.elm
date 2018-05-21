module Utility exposing (..)

import Constant exposing (drawingWindowRatio)
import Dict
import Model exposing (..)


calculateDrawingSpaceEdgePx : Int -> Int -> Float
calculateDrawingSpaceEdgePx windowWidth windowHeight =
    min windowWidth windowHeight
        * drawingWindowRatio
        // 100
        |> toFloat


guaranteeJust : Maybe a -> a
guaranteeJust noLongerOptional =
    case noLongerOptional of
        Just something ->
            something

        Nothing ->
            Debug.crash "noLongerOptional was Nothing after all!"


getFirst : List String -> String
getFirst players =
    guaranteeJust (players |> List.head)


isValidVote : String -> Model -> Bool
isValidVote playerId model =
    isPlayerGameMaster playerId model == False && playerId /= model.playerId


playersExceptMeAndGameMaster : Model -> Dict.Dict String Player
playersExceptMeAndGameMaster model =
    Dict.remove model.playerId model.state.players |> removeGameMaster


removeGameMaster : Dict.Dict String Player -> Dict.Dict String Player
removeGameMaster players =
    let
        playerList =
            players |> Dict.toList

        gameMasterIndex =
            getGameMasterIndex playerList
    in
    Dict.remove gameMasterIndex players


getGameMasterIndex : List ( String, Player ) -> String
getGameMasterIndex playerList =
    playerList |> getGameMasterTuple |> Tuple.first


getGameMaster : List ( String, Player ) -> Player
getGameMaster playerList =
    playerList |> getGameMasterTuple |> Tuple.second


getGameMasterTuple : List ( String, Player ) -> ( String, Player )
getGameMasterTuple playerList =
    (List.filter (\player -> (player |> Tuple.second |> .role) == GameMaster) playerList |> List.head) |> guaranteeJust


getTricksterTuple : List ( String, Player ) -> ( String, Player )
getTricksterTuple playerList =
    (List.filter (\player -> (player |> Tuple.second |> .role) == Trickster) playerList |> List.head) |> guaranteeJust


getTrickster : List ( String, Player ) -> Player
getTrickster playerList =
    playerList |> getTricksterTuple |> Tuple.second


getRole : Model -> Role
getRole model =
    Dict.get model.playerId model.state.players |> guaranteeJust |> .role


getPlayerRole : String -> Model -> Role
getPlayerRole playerId model =
    Dict.get playerId model.state.players |> guaranteeJust |> .role


getColor : Model -> String
getColor model =
    Dict.get model.playerId model.state.players |> guaranteeJust |> .color


getVotedFor : Model -> Maybe String
getVotedFor model =
    Dict.get model.playerId model.state.players |> guaranteeJust |> .votedFor


isGameMaster : Model -> Bool
isGameMaster model =
    getRole model == GameMaster


isTrickster : Model -> Bool
isTrickster model =
    getRole model == Trickster


isArtist : Model -> Bool
isArtist model =
    getRole model == Artist


hasVoted : Model -> Bool
hasVoted model =
    (Dict.get model.playerId model.state.players |> guaranteeJust |> .votedFor) /= Nothing


hasVotedFor : Model -> String -> Bool
hasVotedFor model playerId =
    let
        player_voted_for =
            getVotedFor model
    in
    player_voted_for == Nothing || (player_voted_for |> guaranteeJust) == playerId


isPlayerGameMaster : String -> Model -> Bool
isPlayerGameMaster playerId model =
    getPlayerRole playerId model == GameMaster
