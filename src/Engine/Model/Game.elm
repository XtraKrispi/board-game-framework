module Engine.Model.Game exposing (..)

import Engine.Model.Player exposing (PlayerId)


type alias Context =
    { numberOfPlayers : Int
    }


type TurnStatus
    = EndTurn
    | SameTurn


type alias Snapshot state =
    { state : state
    , context : Context
    , playerId : PlayerId
    }


type alias Move state move =
    move
    -> Snapshot state
    -> ( state, TurnStatus )


type alias Game state move gameOver =
    { setup : () -> state
    , moves : Move state move
    , checkWinner : Snapshot state -> Maybe gameOver
    }


applyMove : Game state move gameOver -> move -> Snapshot state -> ( state, TurnStatus )
applyMove { moves } move state =
    moves move state


isGameOver : Game state move gameOver -> Snapshot state -> Maybe gameOver
isGameOver { checkWinner } state =
    checkWinner state
