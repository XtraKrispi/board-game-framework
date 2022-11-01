module Games.Catego exposing (..)

import Engine.Model.Game exposing (Game, Snapshot, TurnStatus(..))
import Engine.Model.Player exposing (PlayerId)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe.Extra as Maybe
import Random


type alias Well =
    { score : Int
    , player1Amt : Maybe Int
    , player2Amt : Maybe Int
    }


type alias Scorecard =
    List Well


type GameState
    = PreRoll
    | Rolled Int Int


type alias CategoState =
    { currentSeed : Random.Seed
    , scorecard : Scorecard
    , gameState : GameState
    }


type alias GameOver =
    PlayerId


type Move
    = RollDice
    | SelectWell Int


processMove : Move -> Snapshot CategoState -> ( CategoState, TurnStatus )
processMove move { state, playerId } =
    case move of
        RollDice ->
            let
                dieRollGenerator =
                    Random.map2 (\die1 die2 -> ( die1, die2 )) (Random.int 1 6) (Random.int 1 6)

                ( ( d1, d2 ), nextSeed ) =
                    Random.step dieRollGenerator state.currentSeed
            in
            ( { state
                | gameState = Rolled d1 d2
                , currentSeed = nextSeed
              }
            , SameTurn
            )

        SelectWell well ->
            case state.gameState of
                PreRoll ->
                    ( state, SameTurn )

                Rolled d1 d2 ->
                    let
                        newWells =
                            state.scorecard
                                |> List.map
                                    (\w ->
                                        if well == w.score then
                                            if playerId == 0 && Maybe.isNothing w.player1Amt then
                                                { w | player1Amt = Just (d1 + d2) }

                                            else if playerId == 1 && Maybe.isNothing w.player2Amt then
                                                { w | player2Amt = Just (d1 + d2) }

                                            else
                                                w

                                        else
                                            w
                                    )
                    in
                    ( { state
                        | scorecard = newWells
                        , gameState =
                            if newWells == state.scorecard then
                                Rolled d1 d2

                            else
                                PreRoll
                      }
                    , if newWells == state.scorecard then
                        SameTurn

                      else
                        EndTurn
                    )


checkWinner : Snapshot CategoState -> Maybe GameOver
checkWinner { state } =
    let
        allFilled =
            state.scorecard
                |> List.all (\w -> Maybe.isJust w.player1Amt && Maybe.isJust w.player2Amt)
    in
    if allFilled then
        state.scorecard
            |> List.filter (\w -> w.player1Amt /= w.player2Amt)
            |> List.foldr
                (\w ( p1, p2 ) ->
                    let
                        p1Amt =
                            Maybe.withDefault 0 w.player1Amt

                        p2Amt =
                            Maybe.withDefault 0 w.player2Amt
                    in
                    if p1Amt > p2Amt then
                        ( p1 + p1Amt, p2 )

                    else
                        ( p1, p2 + p2Amt )
                )
                ( 0, 0 )
            |> (\( p1, p2 ) ->
                    if p1 > p2 then
                        Just 0

                    else
                        Just 1
               )

    else
        Nothing


catego : Random.Seed -> Game CategoState Move GameOver
catego initialSeed =
    { setup =
        { currentSeed = initialSeed
        , scorecard =
            List.range 2 12
                |> List.map
                    (\s ->
                        { score = s
                        , player1Amt = Nothing
                        , player2Amt = Nothing
                        }
                    )
        , gameState = PreRoll
        }
    , moves = processMove
    , checkWinner = checkWinner
    }


viewGameOver : GameOver -> Snapshot CategoState -> Html Move
viewGameOver go snap =
    Html.div [ Html.Attributes.class "h-full flex flex-col items-center justify-between" ]
        [ Html.div []
            [ Html.div [ Html.Attributes.class "text-center text-[32px]" ]
                [ Html.h1 []
                    [ Html.text "Game Over!"
                    ]
                ]
            , Html.div [ Html.Attributes.class "text-center text-[24px]" ]
                [ Html.h1 []
                    [ Html.text
                        (case go of
                            0 ->
                                "Player 1 won!"

                            1 ->
                                "Player 2 won!"

                            _ ->
                                "Something broke"
                        )
                    ]
                ]
            ]
        , renderScorecard True snap
        , Html.div [] []
        ]


view : Snapshot CategoState -> Html Move
view ({ state, playerId } as snap) =
    Html.div [ Html.Attributes.class "h-full flex flex-col items-center justify-between" ]
        [ Html.div []
            [ Html.div [ Html.Attributes.class "text-center text-[32px]" ]
                [ Html.h1 []
                    [ Html.text "Catego"
                    ]
                ]
            , Html.div [ Html.Attributes.class "text-center text-[24px]" ]
                [ Html.h1 []
                    [ Html.text
                        ("Player "
                            ++ (if playerId == 0 then
                                    "1"

                                else
                                    "2"
                               )
                            ++ "'s turn"
                        )
                    ]
                , case state.gameState of
                    PreRoll ->
                        Html.text ""

                    Rolled d1 d2 ->
                        Html.h2 []
                            [ Html.text ("Total: " ++ String.fromInt (d1 + d2))
                            ]
                ]
            ]
        , Html.div [ Html.Attributes.class "flex flex-col items-center" ]
            [ renderScorecard False snap
            ]
        , Html.div [ Html.Attributes.class "flex flex-col items-center h-40" ] [ renderGameState state.gameState ]
        ]


type WellState
    = Winner
    | Loser
    | Draw


renderScorecard : Bool -> Snapshot CategoState -> Html Move
renderScorecard isGameOver { state, playerId } =
    let
        clickAttr pId well =
            if playerId == pId && state.gameState /= PreRoll then
                [ Html.Attributes.class "cursor-pointer"
                , Html.Events.onClick (SelectWell well.score)
                ]

            else
                []

        getWellState pId well =
            let
                p1Amt =
                    Maybe.withDefault 0 well.player1Amt

                p2Amt =
                    Maybe.withDefault 0 well.player2Amt
            in
            if p1Amt > p2Amt then
                if pId == 0 then
                    Winner

                else
                    Loser

            else if p1Amt < p2Amt then
                if pId == 0 then
                    Loser

                else
                    Winner

            else
                Draw

        getTotal pId =
            state.scorecard
                |> List.map
                    (\w ->
                        if getWellState pId w == Winner then
                            w.score

                        else
                            0
                    )
                |> List.sum
    in
    Html.table []
        [ Html.thead []
            [ Html.tr []
                ((Html.th [ Html.Attributes.class "border p-4" ] [ Html.text "Players" ]
                    :: (state.scorecard
                            |> List.map (\w -> Html.th [ Html.Attributes.class "border p-4" ] [ Html.text (String.fromInt w.score) ])
                       )
                 )
                    ++ (if isGameOver then
                            [ Html.th [ Html.Attributes.class "border p-4" ] [ Html.text "Total" ] ]

                        else
                            []
                       )
                )
            ]
        , Html.tbody []
            [ Html.tr []
                ((Html.td [ Html.Attributes.class "border p-4" ] [ Html.text "Player 1" ]
                    :: (state.scorecard
                            |> List.map
                                (\w ->
                                    let
                                        wellState =
                                            getWellState 0 w
                                    in
                                    Html.td
                                        (Html.Attributes.class "border p-4"
                                            :: Html.Attributes.classList
                                                [ ( "bg-gray-700", isGameOver && wellState == Winner )
                                                , ( "text-white", isGameOver && wellState == Winner )
                                                , ( "line-through", isGameOver && wellState == Draw )
                                                ]
                                            :: clickAttr 0 w
                                        )
                                        [ Html.text
                                            (w.player1Amt
                                                |> Maybe.map String.fromInt
                                                |> Maybe.withDefault ""
                                            )
                                        ]
                                )
                       )
                 )
                    ++ (if isGameOver then
                            [ Html.td [ Html.Attributes.class "border p-4" ] [ Html.text (String.fromInt (getTotal 0)) ] ]

                        else
                            []
                       )
                )
            , Html.tr []
                (Html.td [ Html.Attributes.class "border p-4" ] [ Html.text "Player 2" ]
                    :: (state.scorecard
                            |> List.map
                                (\w ->
                                    let
                                        wellState =
                                            getWellState 1 w
                                    in
                                    Html.td
                                        (Html.Attributes.class "border p-4"
                                            :: Html.Attributes.classList
                                                [ ( "bg-gray-700", isGameOver && wellState == Winner )
                                                , ( "text-white", isGameOver && wellState == Winner )
                                                , ( "line-through", isGameOver && wellState == Draw )
                                                ]
                                            :: clickAttr 1 w
                                        )
                                        [ Html.text
                                            (w.player2Amt
                                                |> Maybe.map String.fromInt
                                                |> Maybe.withDefault ""
                                            )
                                        ]
                                )
                       )
                    ++ (if isGameOver then
                            [ Html.td [ Html.Attributes.class "border p-4" ] [ Html.text (String.fromInt (getTotal 1)) ] ]

                        else
                            []
                       )
                )
            ]
        ]


renderGameState : GameState -> Html Move
renderGameState gs =
    case gs of
        PreRoll ->
            Html.div [] [ Html.button [ Html.Events.onClick RollDice ] [ Html.text "Roll Dice" ] ]

        Rolled d1 d2 ->
            Html.div [ Html.Attributes.class "flex space-x-4" ]
                [ renderDie d1
                , renderDie d2
                ]


renderDie : Int -> Html msg
renderDie num =
    Html.div [ Html.Attributes.class "h-36 w-36 bg-white rounded-xl shadow-xl flex justify-between p-4" ]
        [ Html.div [ Html.Attributes.class "space-y-4 pt-1" ]
            [ Html.div
                [ Html.Attributes.class "w-6 h-6 rounded-full"
                , Html.Attributes.classList [ ( "bg-gray-700", num > 3 ) ]
                ]
                []
            , Html.div
                [ Html.Attributes.class "w-6 h-6  rounded-full"
                , Html.Attributes.classList [ ( "bg-gray-700", num == 6 ) ]
                ]
                []
            , Html.div
                [ Html.Attributes.class "w-6 h-6 rounded-full"
                , Html.Attributes.classList [ ( "bg-gray-700", num > 1 ) ]
                ]
                []
            ]
        , Html.div [ Html.Attributes.class "space-y-4 pt-1" ]
            [ Html.div
                [ Html.Attributes.class "w-6 h-6 rounded-full"
                ]
                []
            , Html.div
                [ Html.Attributes.class "w-6 h-6  rounded-full"
                , Html.Attributes.classList [ ( "bg-gray-700", num == 1 || num == 3 || num == 5 ) ]
                ]
                []
            , Html.div
                [ Html.Attributes.class "w-6 h-6 rounded-full"
                ]
                []
            ]
        , Html.div [ Html.Attributes.class "space-y-4 pt-1" ]
            [ Html.div
                [ Html.Attributes.class "w-6 h-6 rounded-full"
                , Html.Attributes.classList [ ( "bg-gray-700", num > 1 ) ]
                ]
                []
            , Html.div
                [ Html.Attributes.class "w-6 h-6 rounded-full"
                , Html.Attributes.classList [ ( "bg-gray-700", num == 6 ) ]
                ]
                []
            , Html.div
                [ Html.Attributes.class "w-6 h-6 rounded-full"
                , Html.Attributes.classList [ ( "bg-gray-700", num > 3 ) ]
                ]
                []
            ]
        ]
