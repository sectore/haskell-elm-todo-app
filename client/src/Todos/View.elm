module Todos.View exposing (view)

import Html exposing (..)
import Material.List as Lists
import Todo.Update as Todo exposing (Msg(..))
import Todo.View as Todo
import Todos.Model as Todos


view : Todos.Model -> Html Msg
view model =
    Lists.ul
        []
        (List.map Todo.view model.todos)
