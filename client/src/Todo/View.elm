module Todo.View exposing (view)

import Html exposing (..)
import Material.List as Lists
import Todo.Model as Todo
import Todo.Update as Todo exposing (Msg(..))


view : Todo.Model -> Html Msg
view todo =
    Lists.li []
        [ text todo.todoDescription ]
