module Games.TicTacToe exposing (..)

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


type Highlight
    = Highlight
    | DontHighlight


type alias TicTacToeState =
    { cells : List ( Cell, Highlight )
    }


type Move
    = ClickCell Int


type GameOver
    = CatsGame
    | PlayerWon PlayerId (List Int)


playerCell : PlayerId -> Cell
playerCell pId =
    case pId of
        0 ->
            X

        1 ->
            O

        _ ->
            Empty


viewGameOver : GameOver -> Game.Snapshot TicTacToeState -> Html move
viewGameOver go { state } =
    let
        ( txt, updatedCells ) =
            case go of
                CatsGame ->
                    ( "It's a draw!", state.cells )

                PlayerWon pId ids ->
                    ( (if pId == 0 then
                        "Player 1"

                       else
                        "Player 2"
                      )
                        ++ " won!"
                    , state.cells
                        |> List.indexedMap
                            (\i ( c, _ ) ->
                                if List.member i ids then
                                    ( c, Highlight )

                                else
                                    ( c, DontHighlight )
                            )
                    )
    in
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
                        txt
                    ]
                ]
            ]
        , renderGrid Nothing { cells = updatedCells }
        , Html.div [] []
        ]


view : Game.Snapshot TicTacToeState -> Html Move
view { state, playerId } =
    Html.div [ Html.Attributes.class "h-full flex flex-col items-center justify-between" ]
        [ Html.div []
            [ Html.div [ Html.Attributes.class "text-center text-[32px]" ]
                [ Html.h1 []
                    [ Html.text "Tic-Tac-Toe"
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
                ]
            ]
        , renderGrid (Just ClickCell) state
        , Html.div [] [ Html.text "" ]
        ]


renderGrid : Maybe (Int -> move) -> TicTacToeState -> Html move
renderGrid mOnClick state =
    let
        clickAttr i =
            case mOnClick of
                Just onClick ->
                    [ Html.Events.onClick (onClick i)
                    , Html.Attributes.class "cursor-pointer"
                    ]

                Nothing ->
                    [ Html.Attributes.class "cursor-not-allowed" ]
    in
    state.cells
        |> List.indexedMap
            (\i ( c, h ) ->
                Html.div
                    (Html.Attributes.class "bg-white text-center flex justify-center items-center text-[52px]"
                        :: clickAttr i
                    )
                    [ case c of
                        X ->
                            Html.span
                                [ Html.Attributes.class "h-[52px] w-[52px] flex items-center justify-center rounded-full"
                                , Html.Attributes.classList [ ( "bg-black text-white ", h == Highlight ) ]
                                ]
                                [ Html.span [] [ Html.text "X" ] ]

                        O ->
                            Html.span
                                [ Html.Attributes.class "h-[52px] w-[52px] flex items-center justify-center rounded-full"
                                , Html.Attributes.classList [ ( "bg-black text-white ", h == Highlight ) ]
                                ]
                                [ Html.span [] [ Html.text "O" ] ]

                        Empty ->
                            Html.span [ Html.Attributes.class "text-transparent h-[52px] w-[52px]" ] [ Html.text "-" ]
                    ]
            )
        |> Html.div [ Html.Attributes.class "bg-black grid grid-cols-3 gap-2 h-64 w-64" ]


processMove : Game.Move TicTacToeState Move
processMove move { state, playerId } =
    case move of
        ClickCell id ->
            let
                newCells =
                    state.cells
                        |> List.indexedMap
                            (\i ( c, h ) ->
                                if i == id && c == Empty then
                                    ( playerCell playerId, h )

                                else
                                    ( c, h )
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


checkWinner :
    { state : TicTacToeState
    , context : Context
    , playerId : PlayerId
    }
    -> Maybe GameOver
checkWinner { state, playerId } =
    let
        rows =
            [ [ 0, 1, 2 ], [ 3, 4, 5 ], [ 6, 7, 8 ] ]

        cols =
            [ [ 0, 3, 6 ], [ 1, 4, 7 ], [ 2, 5, 8 ] ]

        diags =
            [ [ 0, 4, 8 ], [ 2, 4, 6 ] ]

        allCombos =
            List.concat [ rows, cols, diags ]

        winners =
            allCombos
                |> List.map
                    (\ids ->
                        ids
                            |> List.map (\id -> List.Extra.getAt id state.cells |> Maybe.map (\c -> ( id, c )))
                            |> Maybe.Extra.values
                            |> (\l -> ( List.length l == 3 && List.length (List.Extra.unique (l |> List.map Tuple.second)) == 1 && List.all (\( id, ( i, _ ) ) -> i == playerCell playerId) l, l ))
                    )
                |> List.filter Tuple.first

        allFilled =
            state.cells
                |> List.all (\( c, _ ) -> c /= Empty)
    in
    if not (List.isEmpty winners) then
        Just <|
            PlayerWon playerId
                (winners
                    |> List.map
                        (\( valid, cells ) ->
                            if valid then
                                List.map Tuple.first cells

                            else
                                []
                        )
                    |> List.concat
                )

    else if allFilled then
        Just <| CatsGame

    else
        Nothing


ticTacToe : Game TicTacToeState Move GameOver
ticTacToe =
    { setup = { cells = List.repeat 9 ( Empty, DontHighlight ) }
    , moves = processMove
    , checkWinner = checkWinner
    }
