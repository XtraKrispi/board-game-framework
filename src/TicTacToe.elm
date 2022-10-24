module TicTacToe exposing (..)

import Engine.Model.Game as Game exposing (Context, Game, TurnStatus(..))
import Engine.Model.Player exposing (PlayerId)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra
import Maybe.Extra


type Cell
    = X
    | O
    | Empty


type alias TicTacToeState =
    { cells : List Cell
    }


type Move
    = ClickCell Int


type GameOver
    = CatsGame
    | PlayerWon PlayerId


playerCell : PlayerId -> Cell
playerCell pId =
    case pId of
        0 ->
            X

        1 ->
            O

        _ ->
            Empty


viewGameOver : GameOver -> TicTacToeState -> Context -> Html move
viewGameOver go state context =
    Html.div [ Html.Attributes.class "flex flex-col items-center" ]
        [ Html.div [ Html.Attributes.class "text-[32px]" ]
            [ Html.h1 []
                [ Html.text "Game Over!"
                ]
            ]
        , Html.div [ Html.Attributes.class "text-[32px]" ]
            [ Html.h1 []
                [ Html.text
                    (case go of
                        CatsGame ->
                            "It's a draw!"

                        PlayerWon pId ->
                            (if pId == 0 then
                                "Player 1"

                             else
                                "Player 2"
                            )
                                ++ " won!"
                    )
                ]
            ]
        , renderGrid Nothing state
        ]


view : TicTacToeState -> Context -> Html Move
view state context =
    Html.div [ Html.Attributes.class "flex flex-col items-center" ]
        [ Html.div [ Html.Attributes.class "text-[32px]" ]
            [ Html.h1 []
                [ Html.text "Tic-Tac-Toe"
                ]
            ]
        , renderGrid (Just ClickCell) state
        ]


renderGrid : Maybe (Int -> move) -> TicTacToeState -> Html move
renderGrid mOnClick state =
    let
        clickAttr i =
            case mOnClick of
                Just onClick ->
                    [ Html.Events.onClick (onClick i) ]

                Nothing ->
                    []
    in
    state.cells
        |> List.indexedMap
            (\i c ->
                Html.div (Html.Attributes.class "border text-center flex justify-center items-center text-xl" :: clickAttr i)
                    [ case c of
                        X ->
                            Html.span [] [ Html.text "X" ]

                        O ->
                            Html.span [] [ Html.text "O" ]

                        Empty ->
                            Html.span [ Html.Attributes.class "text-transparent" ] [ Html.text "-" ]
                    ]
            )
        |> Html.div [ Html.Attributes.class "p-4 grid grid-cols-3 h-64 w-64" ]


processMove : Game.Move TicTacToeState Move
processMove move { state, playerId } =
    case move of
        ClickCell id ->
            let
                newCells =
                    state.cells
                        |> List.indexedMap
                            (\i c ->
                                if i == id && c == Empty then
                                    playerCell playerId

                                else
                                    c
                            )
            in
            ( { state
                | cells =
                    newCells
              }
            , if state.cells == newCells then
                Game.SameTurn

              else
                Game.EndTurn
            )


ticTacToe : Game TicTacToeState Move GameOver
ticTacToe =
    { setup = \_ -> { cells = List.repeat 9 Empty }
    , moves = processMove
    , checkWinner =
        \{ state, context, playerId } ->
            let
                rows =
                    [ [ 0, 1, 2 ], [ 3, 4, 5 ], [ 6, 7, 8 ] ]

                cols =
                    [ [ 0, 3, 6 ], [ 1, 4, 7 ], [ 2, 5, 8 ] ]

                diags =
                    [ [ 0, 4, 8 ], [ 2, 4, 6 ] ]

                allCombos =
                    List.concat [ rows, cols, diags ]

                anyWinners =
                    allCombos
                        |> List.map
                            (\ids ->
                                ids
                                    |> List.map (\id -> List.Extra.getAt id state.cells)
                                    |> Maybe.Extra.values
                                    |> (\l -> List.length l == 3 && List.length (List.Extra.unique l) == 1 && List.all (\i -> i == playerCell playerId) l)
                            )
                        |> List.any identity

                allFilled =
                    state.cells
                        |> List.all (\c -> c /= Empty)
            in
            if anyWinners then
                Just <| PlayerWon playerId

            else if allFilled then
                Just <| CatsGame

            else
                Nothing
    }



-- [0,1,2], [3,4,5], [6,7,8]
-- [0,3,6], [1,4,7], [2,5,8]
-- [0,4,8], [2,4,6]
