module Main exposing (..)

import Html.App as Html
import Model exposing (Model, initialModel)
import Todos.Api as Todos
import Update exposing (update, Msg(..))
import View exposing (view)


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.map TodosMsg Todos.get )
