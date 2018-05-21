module View exposing (view)

import Bulma.CDN exposing (stylesheet)
import Bulma.Columns exposing (Display(..), Gap(..), column, columnModifiers, columns, columnsModifiers)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (Color(Danger, Dark, Info, Warning), HorizontalAlignment(Left), Size(Large), fullWidth)
import Constant exposing (viewBoxLength)
import Dict
import Html exposing (Html, br, div, h2, li, main_, text, ul)
import Html.Attributes exposing (attribute, class, id, placeholder, style, type_)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Model exposing (BigState(..), Line, LittleState(..), Model, Msg(..), Player, Point, Role(..))
import Mouse exposing (onContextMenu)
import Pointer
import Svg exposing (Svg, polyline, svg)
import Svg.Attributes exposing (fill, pointerEvents, points, stroke, strokeWidth, viewBox)
import Utility exposing (..)


view : Model -> Html Msg
view model =
    Html.main_
        [ style
            [ ( "height", "100vh" )
            , ( "touch-action", "none" )
            ]
        ]
        [ stylesheet
        , roleView model
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
    lobbyView model


lobbyView : Model -> Html Msg
lobbyView model =
    if model.hasEnteredName then
        viewGeneralLayout model
    else
        container [] <| enterNameView model


enterNameView : Model -> List (Html Msg)
enterNameView model =
    [ container []
        [ soloDrawingSpace model
        ]
    , br [] []
    , container []
        [ button myButtonModifiers [ onClick ChooseName ] [ text "Pick Name" ]
        ]
    ]


startGame : Model -> Html Msg
startGame model =
    if isActivePlayer model && (getPlayersWithNames model |> List.length) >= 3 then
        button myButtonModifiers [ onClick PushStartGame ] [ text "go to Game" ]
    else
        text ""


littleStateView : Model -> List (Html Msg)
littleStateView model =
    case model.state.littleState of
        Pick ->
            [ container [] <| choicesView model ]

        Draw ->
            [ container
                [ style []
                ]
                [ sharedDrawingSpace model ]
            , br [] []
            , container [] [ viewSubject model ]
            ]

        Vote ->
            let
                extraParts =
                    if isGameMaster model || isTrickster model || hasVoted model == True then
                        [ title H3 [ coolStyle ] [ text "Wait for all the players to vote" ] ]
                    else
                        [ title H3 [ coolStyle ] [ text "Click the player you think was the trickster!" ] ]
            in
            extraParts ++ [ viewFinishedPainting model ]

        Tricky ->
            if isTrickster model then
                tricksterPad model
            else
                []

        Check ->
            let
                extraParts =
                    if isGameMaster model then
                        [ text "Was the trickster's guess correct?"
                        , button myButtonModifiers [ onClick <| Validate True ] [ text "Yes" ]
                        , button myButtonModifiers [ onClick <| Validate False ] [ text "No" ]
                        ]
                    else
                        []
            in
            extraParts ++ [ viewGuess model ]

        _ ->
            [ text "" ]


tricksterPad : Model -> List (Html Msg)
tricksterPad model =
    [ container []
        [ button myButtonModifiers [ onClick GuessSubject ] [ text "Guess Topic?" ]
        ]
    , br [] []
    , container []
        [ soloDrawingSpace model
        ]
    ]


viewRest : Model -> Html Msg
viewRest model =
    case model.state.bigState of
        Welcome ->
            viewWelcome model

        Lobby ->
            viewLobby model

        Game ->
            viewGameGeneralLayout model

        End ->
            div []
                [ displayWinner model
                , viewSubject model
                , viewFinishedPainting model
                ]


viewGeneralLayout : Model -> Html Msg
viewGeneralLayout model =
    columns { columnsModifiers | gap = Gap0 }
        [ style [ ( "height", "100%" ) ] ]
        [ column
            columnModifiers
            [ id "names"
            , class "is-one-third"
            ]
            [ columns { columnsModifiers | gap = Gap0, multiline = True }
                [ style [ ( "height", "100%" ) ]
                , class "is-mobile"
                ]
                (displayPlayers model)
            ]
        , column
            columnModifiers
            [ id "art" ]
            [ columns { columnsModifiers | gap = Gap0, display = MobileAndBeyond }
                [ style [ ( "height", "100%" ) ] ]
                [ -- TODO add collaborative "lobby pad" to draw for fun
                  startGame model
                ]
            ]
        ]


viewGameGeneralLayout : Model -> Html Msg
viewGameGeneralLayout model =
    columns { columnsModifiers | gap = Gap0 }
        [ style [ ( "height", "100%" ) ] ]
        [ column
            columnModifiers
            [ id "names"
            , class "is-one-third"
            ]
            [ columns { columnsModifiers | gap = Gap0, multiline = True }
                [ style [ ( "height", "100%" ) ]
                , class "is-mobile"
                ]
                (displayPlayers model)
            ]
        , column
            columnModifiers
            [ id "art" ]
            [ columns { columnsModifiers | gap = Gap0, display = MobileAndBeyond }
                [ style
                    [ ( "height", "100%" )
                    ]
                ]
                [ column columnModifiers [] (littleStateView model) ]
            ]
        ]


displayWinner : Model -> Html msg
displayWinner model =
    text (Maybe.withDefault "" model.state.winner)


isActivePlayer : Model -> Bool
isActivePlayer model =
    List.member model.playerId model.state.activePlayers


isPlayerActivePlayer : Model -> String -> Bool
isPlayerActivePlayer model playerId =
    List.member playerId model.state.activePlayers


choicesView : Model -> List (Html Msg)
choicesView model =
    if isActivePlayer model then
        [ container []
            [ soloDrawingSpace model
            ]
        , br [] []
        , container []
            [ button myButtonModifiers [ onClick ChooseSubject ] [ text "Submit Subject" ]
            ]
        ]
    else
        []


getViewBox : Model -> Html.Attribute msg
getViewBox model =
    viewBox <| "0 0 " ++ toString viewBoxLength ++ " " ++ toString viewBoxLength


sharedDrawingSpace : Model -> Html Msg
sharedDrawingSpace model =
    drawingSpaceWithRatio (sharedDrawingSpaceAttributes model) 0.9 (drawPainting model) model


viewFinishedPainting : Model -> Html Msg
viewFinishedPainting model =
    drawingSpaceWithRatio (readOnlyRenderAttributes model False) 1.0 (drawPainting model) model


soloDrawingSpace : Model -> Html Msg
soloDrawingSpace model =
    let
        myColor =
            getColor model

        lines =
            drawLines (model.currentLine :: model.currentSoloDrawing) myColor
    in
    drawingSpaceWithRatio (soloDrawingSpaceAttributes model) 1.0 lines model


nameTagViewingSpace : Model -> Player -> String -> Bool -> Html Msg
nameTagViewingSpace model player player_id shouldHighlight =
    drawingSpaceForVoting (readOnlyRenderAttributes model shouldHighlight) 0.2 (drawLines player.name player.color) model player_id


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
        drawingSpaceWithRatio (readOnlyRenderAttributes model False) 0.2 lines model


viewGuess : Model -> Html Msg
viewGuess model =
    let
        tricksterColor =
            model.state.players |> Dict.toList |> getTrickster |> .color

        lines =
            drawLines model.state.guess tricksterColor
    in
    drawingSpaceWithRatio (readOnlyRenderAttributes model False) 1.0 lines model


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


readOnlyRenderAttributes : Model -> Bool -> List (Html.Attribute Msg)
readOnlyRenderAttributes model shouldHighlight =
    [ getViewBox model
    , onContextMenu disableContextMenu
    , getReadOnlyStyle shouldHighlight
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
                    Just <| polyline [ pointerEvents "none", points (pointString line), stroke color, strokeWidth "1em", fill "none" ] []
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
        [ columns columnsModifiers [] <| displayPlayers model
        ]


getPlayersWithNames : Model -> List Player
getPlayersWithNames model =
    getPlayersWithNamesTuple model |> List.map Tuple.second


getPlayersWithNamesTuple : Model -> List ( String, Player )
getPlayersWithNamesTuple model =
    Dict.toList model.state.players |> List.filter hasName


hasName : ( String, Player ) -> Bool
hasName ( id, player ) =
    player.name /= []


displayPlayers : Model -> List (Html Msg)
displayPlayers model =
    List.map
        (\( player_id, player ) ->
            let
                shouldHighlight =
                    shouldHighlightPlayer model player_id
            in
            column
                columnModifiers
                [ class "is-one-third" ]
                [ nameTagViewingSpace model player player_id shouldHighlight ]
        )
    <|
        getPlayersWithNamesTuple model


shouldHighlightPlayer : Model -> String -> Bool
shouldHighlightPlayer model playerId =
    case model.state.littleState of
        Vote ->
            if hasVoted model then
                hasVotedFor model playerId
            else
                -- tricksters and game masters can't vote
                isArtist model && isValidVote playerId model

        _ ->
            isPlayerActivePlayer model playerId


getReadOnlyStyle : Bool -> Html.Attribute msg
getReadOnlyStyle shouldHighlight =
    let
        readOnlyStyle =
            []
    in
    if shouldHighlight then
        style <| [ ( "box-shadow", "0px -1px 31px 6px  rgba(237,7,7,1)" ) ] ++ readOnlyStyle
    else
        style readOnlyStyle


pointString : List Point -> String
pointString points =
    String.join " " points


disableContextMenu : a -> Msg
disableContextMenu event =
    None


coolStyle : Html.Attribute Msg
coolStyle =
    style [ ( "color", "#F5F5F5" ) ]


roleView : Model -> Html Msg
roleView model =
    case model.state.bigState of
        Lobby ->
            title H3 [ coolStyle ] [ text <| Maybe.withDefault "" model.tableTopic ]

        Game ->
            title H3 [ coolStyle ] <| [ getRole model |> toString |> text ]

        _ ->
            text ""
