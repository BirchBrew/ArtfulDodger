module View exposing (view)

import Bulma.CDN exposing (stylesheet)
import Bulma.Columns exposing (column, columnModifiers, columns, columnsModifiers)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (Color(Danger, Dark, Info, Warning), HorizontalAlignment(Left), Size(Large), fullWidth)
import Constant exposing (viewBoxLength)
import Dict
import Html exposing (Html, br, div, h2, li, main_, text, ul)
import Html.Attributes exposing (attribute, placeholder, style, type_)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Model exposing (BigState(..), LittleState(..), Model, Msg(..), Player, Point, Role(..))
import Mouse exposing (onContextMenu)
import Pointer
import Svg exposing (Svg, polyline, svg)
import Svg.Attributes exposing (fill, points, stroke, strokeWidth, viewBox)


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
                    , startGame model
                    ]
                ]
            ]
        ]


startGame : Model -> Html Msg
startGame model =
    if isActivePlayer model && (model.state.players |> Dict.size) >= 3 then
        button myButtonModifiers [ onClick PushStartGame ] [ text "go to Game" ]
    else
        text ""


viewGame : Model -> Html Msg
viewGame model =
    hero { heroModifiers | size = Large, color = Dark }
        []
        [ heroBody
            [ style [ ( "justify-content", "center" ) ]
            ]
            [ container [] [ roleView model ]
            , container [] <| littleStateView model
            ]
        ]


littleStateView : Model -> List (Button Msg)
littleStateView model =
    case model.state.littleState of
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

        Tricky ->
            if isTrickster model then
                [ button myButtonModifiers [ onClick GuessTopic ] [ text "Guess Topic" ] ]
            else
                []

        Check ->
            if isGameMaster model then
                [ text "Was the trickster's guess correct?"
                , button myButtonModifiers [ onClick <| Validate True ] [ text "Yes" ]
                , button myButtonModifiers [ onClick <| Validate False ] [ text "No" ]
                ]
            else
                []


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
    Dict.remove (gameMaster |> guaranteeJust |> Tuple.first) players


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


roleView : Model -> Html Msg
roleView model =
    getRole model |> toString |> text


getRole : Model -> Role
getRole model =
    Dict.get model.playerId model.state.players |> guaranteeJust |> .role


isGameMaster : Model -> Bool
isGameMaster model =
    getRole model == GameMaster


isTrickster : Model -> Bool
isTrickster model =
    getRole model == Trickster


isBasicPlayer : Model -> Bool
isBasicPlayer model =
    getRole model == BasicPlayer


hasVoted : Model -> Bool
hasVoted model =
    (Dict.get model.playerId model.state.players |> guaranteeJust |> .votedFor) /= Nothing
