module Main exposing (main)

import Browser
import Engine.Engine exposing (Client, initClient)
import Html exposing (Html)
import TicTacToe


type alias Model =
    { client : Client TicTacToe.TicTacToeState TicTacToe.Move TicTacToe.GameOver
    }


type Msg
    = GameMove TicTacToe.Move


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { client =
            initClient { numberOfPlayers = 2 }
                TicTacToe.ticTacToe
                TicTacToe.view
                TicTacToe.viewGameOver
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameMove move ->
            ( { client = Engine.Engine.update model.client move }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view { client } =
    Html.div [] [ Html.map GameMove (Engine.Engine.view client) ]
