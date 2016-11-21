module Main exposing (..)

import Html
import App.View as App
import App.State as App
import App.Types as App


main : Program Never App.Model App.Msg
main =
    Html.program
        { init = ( App.initialModel, App.initialCommand )
        , view = App.root
        , update = App.update
        , subscriptions = always Sub.none
        }
