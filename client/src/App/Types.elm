module App.Types exposing (..)

import Todos.Types as Ts
import Todo.Types as T


type alias Model =
    { todos : Ts.Todos
    , newTodo : T.NewTodo
    }


type Msg
    = TodosMsg Ts.Msg
    | TodoMsg T.Msg
