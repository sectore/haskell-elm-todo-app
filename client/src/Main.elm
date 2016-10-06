module Main exposing (..)

import Html.App as Html
import App.View as View
import App.State as State


main : Program Never
main =
    Html.program
        { init = ( State.initialModel, State.initialCommand )
        , view = View.root
        , update = State.update
        , subscriptions = always Sub.none
        }
