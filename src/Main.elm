module Main exposing (main)

import Browser
import Engine.Engine exposing (Client, initClient)
import Games.Catego as Catego
import Games.TicTacToe as TicTacToe
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Random


type Game
    = Tic
    | Cat


type Model
    = GettingSeed
    | ReadyToPlay InGameModel


type alias InGameModel =
    { ticTacToeClient : Client TicTacToe.TicTacToeState TicTacToe.Move TicTacToe.GameOver
    , categoClient : Client Catego.CategoState Catego.Move Catego.GameOver
    , selectedGame : Game
    }


type Msg
    = TicTacToeMove TicTacToe.Move
    | CategoMove Catego.Move
    | GotSeed Random.Seed
    | SwitchGame Game


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( GettingSeed, Random.generate GotSeed Random.independentSeed )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        ReadyToPlay mdl ->
            case msg of
                TicTacToeMove move ->
                    ( ReadyToPlay { mdl | ticTacToeClient = Engine.Engine.update mdl.ticTacToeClient move }
                    , Cmd.none
                    )

                CategoMove move ->
                    ( ReadyToPlay { mdl | categoClient = Engine.Engine.update mdl.categoClient move }
                    , Cmd.none
                    )

                SwitchGame game ->
                    ( ReadyToPlay { mdl | selectedGame = game }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GettingSeed ->
            case msg of
                GotSeed seed ->
                    ( ReadyToPlay
                        { ticTacToeClient =
                            initClient { numberOfPlayers = 2 }
                                TicTacToe.ticTacToe
                                TicTacToe.view
                                TicTacToe.viewGameOver
                        , categoClient =
                            initClient { numberOfPlayers = 2 }
                                (Catego.catego seed)
                                Catego.view
                                Catego.viewGameOver
                        , selectedGame = Tic
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { body =
        [ Html.div [ Html.Attributes.class "h-screen" ]
            [ case model of
                GettingSeed ->
                    Html.div [] []

                ReadyToPlay mdl ->
                    inGameView mdl
            ]
        ]
    , title = "Board Game Framework"
    }


tab : Bool -> Game -> Html Msg
tab isActive game =
    let
        gameText =
            case game of
                Tic ->
                    "Tic Tac Toe"

                Cat ->
                    "Catego"

        evtAttr =
            if isActive then
                []

            else
                [ Html.Events.onClick (SwitchGame game) ]
    in
    Html.li [ Html.Attributes.class "mr-2" ]
        [ Html.a
            ([ Html.Attributes.href "#"
             , Html.Attributes.class "inline-block p-4 rounded-t-lg border-b-2"
             , Html.Attributes.classList
                [ ( "text-blue-600", isActive )
                , ( "border-blue-600", isActive )
                , ( "active", isActive )
                , ( "border-transparent", not isActive )
                , ( "hover:text-gray-600", not isActive )
                , ( "hover:border-gray-300", not isActive )
                ]
             ]
                ++ evtAttr
            )
            [ Html.text gameText ]
        ]


inGameView : InGameModel -> Html Msg
inGameView { ticTacToeClient, categoClient, selectedGame } =
    Html.div [ Html.Attributes.class "h-screen flex flex-col items-center" ]
        [ Html.div [ Html.Attributes.class "text-sm font-medium text-center text-gray-500 border-b border-gray-200" ]
            [ Html.ul [ Html.Attributes.class "flex flex-wrap -mb-px" ]
                [ tab (selectedGame == Tic) Tic
                , tab (selectedGame == Cat) Cat
                ]
            ]
        , case selectedGame of
            Tic ->
                Html.div [ Html.Attributes.class "grow" ] [ Html.map TicTacToeMove (Engine.Engine.view ticTacToeClient) ]

            Cat ->
                Html.div [ Html.Attributes.class "grow" ] [ Html.map CategoMove (Engine.Engine.view categoClient) ]
        ]



{-




-}
