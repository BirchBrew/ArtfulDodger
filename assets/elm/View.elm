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
import Model exposing (BigState(..), Line, LittleState(..), Model, Msg(..), Player, Point, Role(..))
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
            (welcomeView model)
        ]


welcomeView : Model -> List (Html Msg)
welcomeView model =
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
                [ tableInput model
                , joinTableButton model
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
            [ lobbyView model
            ]
        ]


lobbyView : Model -> Html Msg
lobbyView model =
    if model.hasEnteredName then
        container []
            [ section Spaced
                []
                [ playersListView model
                , startGame model
                ]
            ]
    else
        container [] <| enterNameView model


enterNameView : Model -> List (Html Msg)
enterNameView model =
    [ container []
        [ button myButtonModifiers [ onClick ChooseName ] [ text "Pick Name" ]
        ]
    , br [] []
    , container []
        [ soloDrawingSpace model
        ]
    ]


startGame : Model -> Html Msg
startGame model =
    if isActivePlayer model && (getPlayersWithNames model |> List.length) >= 3 then
        button myButtonModifiers [ onClick PushStartGame ] [ text "go to Game" ]
    else
        text ""


viewGame : Model -> Html Msg
viewGame model =
    hero { heroModifiers | size = Large, color = Dark }
        []
        [ heroHead []
            [ container []
                [ roleView model
                , playersListView model
                ]
            ]
        , heroBody
            [ style [ ( "justify-content", "center" ) ]
            ]
            (littleStateView model)
        ]


littleStateView : Model -> List (Html Msg)
littleStateView model =
    case model.state.littleState of
        Pick ->
            choicesView model

        Draw ->
            [ viewSubject model
            , sharedDrawingSpace model
            ]

        Vote ->
            if isGameMaster model || hasVoted model == True then
                []
            else
                [ text "Click the player you think was the trickster!" ]

        Tricky ->
            if isTrickster model then
                [ button myButtonModifiers [ onClick GuessSubject ] [ text "Guess Topic" ] ]
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

        _ ->
            [ text "" ]


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


choicesView : Model -> List (Html Msg)
choicesView model =
    if isActivePlayer model then
        [ container []
            [ button myButtonModifiers [ onClick ChooseSubject ] [ text "Submit Subject" ]
            ]
        , br [] []
        , container []
            [ soloDrawingSpace model
            ]
        ]
        -- [ columns columnsModifiers
        --     []
        --     [ column columnModifiers
        --         []
        --         [ button myButtonModifiers [ onClick ChooseSubject ] [ text "Submit Subject" ]
        --         ]
        --     , column columnModifiers
        --         []
        --         [ soloDrawingSpace model
        --         ]
        --     ]
        -- ]
    else
        []


getViewBox : Model -> Html.Attribute msg
getViewBox model =
    viewBox <| "0 0 " ++ toString viewBoxLength ++ " " ++ toString viewBoxLength


sharedDrawingSpace : Model -> Html Msg
sharedDrawingSpace model =
    drawingSpaceWithRatio (sharedDrawingSpaceAttributes model) 1.0 (drawPainting model) model


soloDrawingSpace : Model -> Html Msg
soloDrawingSpace model =
    let
        myColor =
            getColor model

        lines =
            drawLines (model.currentLine :: model.currentSoloDrawing) myColor
    in
    drawingSpaceWithRatio (soloDrawingSpaceAttributes model) 1.0 lines model


nameTagViewingSpace : Model -> Player -> String -> Html Msg
nameTagViewingSpace model player player_id =
    drawingSpaceForVoting (readOnlyRenderAttributes model) 0.1 (drawLines player.name player.color) model player_id


viewSubject : Model -> Html Msg
viewSubject model =
    if isTrickster model then
        text ""
    else
        let
            gameMasterColor =
                model.state.players |> Dict.toList |> getGameMaster |> .color

            lines =
                drawLines model.state.subject gameMasterColor
        in
        drawingSpaceWithRatio (readOnlyRenderAttributes model) 0.2 lines model


drawingSpaceWithRatio : List (Html.Attribute Msg) -> Float -> List (Svg Msg) -> Model -> Html Msg
drawingSpaceWithRatio attributes ratio lines model =
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
        [ svg attributes lines
        ]


drawingSpaceForVoting : List (Html.Attribute Msg) -> Float -> List (Svg Msg) -> Model -> String -> Html Msg
drawingSpaceForVoting attributes ratio lines model player_id =
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
        , onClick <| VoteFor player_id
        ]
        [ svg attributes lines
        ]


sharedDrawingSpaceAttributes : Model -> List (Html.Attribute Msg)
sharedDrawingSpaceAttributes model =
    [ getViewBox model

    -- pointer capture hack to continue "globally" the event anywhere on document.
    , attribute "onpointerdown" "event.target.setPointerCapture(event.pointerId);"
    , onContextMenu disableContextMenu
    ]
        ++ maybeListenForMove model


soloDrawingSpaceAttributes : Model -> List (Html.Attribute Msg)
soloDrawingSpaceAttributes model =
    [ getViewBox model

    -- pointer capture hack to continue "globally" the event anywhere on document.
    , attribute "onpointerdown" "event.target.setPointerCapture(event.pointerId);"
    , onContextMenu disableContextMenu
    ]
        ++ maybeListenForSoloMove model


maybeListenForSoloMove : Model -> List (Html.Attribute Msg)
maybeListenForSoloMove model =
    let
        defaultList =
            [ Pointer.onDown Down
            , Pointer.onUp UpWithFreedom
            ]
    in
    if isActivePlayer model || (model.state.bigState == Lobby && model.hasEnteredName == False) then
        case model.mouseDown of
            True ->
                Pointer.onMove MoveWithFreedom :: defaultList

            False ->
                defaultList
    else
        []


readOnlyRenderAttributes : Model -> List (Html.Attribute Msg)
readOnlyRenderAttributes model =
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
    if isDrawing model || isWritingName model then
        case model.mouseDown of
            True ->
                Pointer.onMove Move :: defaultList

            False ->
                defaultList
    else
        []


isDrawing : Model -> Bool
isDrawing model =
    isActivePlayer model && model.state.littleState == Draw


isWritingName : Model -> Bool
isWritingName model =
    model.state.bigState == Lobby && model.hasEnteredName == False


drawPainting : Model -> List (Svg msg)
drawPainting { state } =
    let
        sortedPlayers =
            state.players
                |> Dict.values
                |> List.sortBy .seat

        svgLines =
            List.map (\{ color, paintLines } -> svgLinesHelper color paintLines) sortedPlayers

        ( firstLines, secondLines ) =
            List.foldr svgLinesFolder ( [], [] ) svgLines
    in
    firstLines ++ secondLines


drawLines : List Line -> String -> List (Svg msg)
drawLines lines color =
    let
        svgLines =
            -- TODO use player color here instead!
            svgLinesHelper color lines
    in
    svgLines


svgLinesHelper : String -> List Line -> List (Svg msg)
svgLinesHelper color lines =
    List.filterMap
        (\line ->
            case line of
                [] ->
                    Nothing

                _ ->
                    Just <| polyline [ points (pointString line), stroke color, strokeWidth "1em", fill "none" ] []
        )
        lines


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


playersListView : Model -> Html Msg
playersListView model =
    div []
        [ h2 [] [ text "Painters" ]
        , ul [] <| displayPlayers model
        ]



-- displayPlayer : List Player -> List (Html.Html msg)


getPlayersWithNames : Model -> List Player
getPlayersWithNames model =
    getPlayersWithNamesTuple model |> List.map Tuple.second


getPlayersWithNamesTuple : Model -> List ( String, Player )
getPlayersWithNamesTuple model =
    Dict.toList model.state.players |> List.filter hasName


hasName : ( String, Player ) -> Bool
hasName ( id, player ) =
    player.name /= []


displayPlayers : Model -> List (Html.Html Msg)
displayPlayers model =
    List.map
        (\( player_id, player ) ->
            li []
                [ nameTagViewingSpace model player player_id ]
        )
    <|
        getPlayersWithNamesTuple model


pointString : List Point -> String
pointString points =
    String.join " " points


disableContextMenu : a -> Msg
disableContextMenu event =
    None


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


roleView : Model -> Html Msg
roleView model =
    title H2 [] <| [ getRole model |> toString |> text ]


getRole : Model -> Role
getRole model =
    Dict.get model.playerId model.state.players |> guaranteeJust |> .role


getColor : Model -> String
getColor model =
    Dict.get model.playerId model.state.players |> guaranteeJust |> .color


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
