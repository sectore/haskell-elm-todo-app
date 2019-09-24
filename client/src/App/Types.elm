module App.Types exposing (..)

import Todo.Types as Todo
import Todos.Types as Todos


type alias Model =
    { todos : Todos.Todos
    , todosVisibility : Todos.Visibility
    , newTodo : Todo.Todo
    }


type Msg
    = TodosMsg Todos.Msg
    | TodoMsg Todo.Msg
