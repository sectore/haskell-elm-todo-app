module Todos.Types exposing (..)

import Http
import Todo.Types as Todo


type alias TodoItem =
    { todo : Todo.Todo
    , description : String
    , editable : Bool
    }


type alias Todos =
    List TodoItem


type Msg
    = ToggleTodoDone TodoItem
    | EditTodo TodoItem
    | CancelEditTodo TodoItem
    | UpdateDescription TodoItem String
    | SaveTodo TodoItem
    | DeleteTodo TodoItem
    | FetchTodosFail Http.Error
    | FetchTodosDone (List Todo.Todo)
