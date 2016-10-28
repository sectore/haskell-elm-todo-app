module App.Types exposing (..)

import Todos.Types as Todos
import Todo.Types as Todo


type alias Model =
    { todos : Todos.Todos
    , todosVisibility : Todos.Visibility
    , newTodo : Todo.Todo
    }


type Msg
    = TodosMsg Todos.Msg
    | TodoMsg Todo.Msg
