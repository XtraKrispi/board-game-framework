module Engine.Engine exposing (Client, initClient, update, view)

import Engine.Model.Game as Game exposing (Context, Game)
import Engine.Model.Player exposing (PlayerId)
import Html exposing (Html)
import Maybe.Extra exposing (isJust)


type alias Render state move =
    Game.Snapshot state -> Html move


type Status gameOver
    = Playing
    | GameOver gameOver


type Client state move gameOver
    = Client
        { context : Context
        , playerId : PlayerId
        , game : Game state move gameOver
        , render : Render state move
        , renderGameOver : gameOver -> Render state move
        , state : state
        , status : Status gameOver
        }


initClient :
    { numberOfPlayers : Int }
    -> Game state move gameOver
    -> Render state move
    -> (gameOver -> Render state move)
    -> Client state move gameOver
initClient { numberOfPlayers } game render renderGameOver =
    let
        initState =
            game.setup
    in
    Client
        { context =
            { numberOfPlayers = numberOfPlayers
            }
        , playerId = 0
        , game = game
        , render = render
        , renderGameOver = renderGameOver
        , state = initState
        , status = Playing
        }


view : Client state move gameOver -> Html move
view (Client client) =
    case client.status of
        Playing ->
            client.render (Game.Snapshot client.state client.context client.playerId)

        GameOver go ->
            client.renderGameOver go (Game.Snapshot client.state client.context client.playerId)


update : Client state move gameOver -> move -> Client state move gameOver
update (Client c) m =
    let
        ( newState, turnStatus ) =
            Game.applyMove c.game m { state = c.state, context = c.context, playerId = c.playerId }

        mGameOver =
            Game.isGameOver c.game
                { state = newState
                , context = c.context
                , playerId = c.playerId
                }

        newPlayerId =
            case turnStatus of
                Game.EndTurn ->
                    modBy c.context.numberOfPlayers (c.playerId + 1)

                Game.SameTurn ->
                    c.playerId
    in
    Client
        { c
            | state = newState
            , playerId =
                if isJust mGameOver then
                    c.playerId

                else
                    newPlayerId
            , status =
                case mGameOver of
                    Just go ->
                        GameOver go

                    Nothing ->
                        Playing
        }
