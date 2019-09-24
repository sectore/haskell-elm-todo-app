module Main exposing (..)

import App.State as App
import App.Types as App
import App.View as App
import Browser


main : Program () App.Model App.Msg
main =
    Browser.element
        { init = \_ -> ( App.initialModel, App.initialCommand )
        , view = App.root
        , update = App.update
        , subscriptions = always Sub.none
        }
