module Todos.Types exposing (..)

import Http
import Todo.Types exposing (..)


type alias TodoItem =
    { todo : Todo
    , editable : Bool
    }


type alias Todos =
    List TodoItem


type Msg
    = FetchTodosDone (List Todo)
    | FetchTodosFail Http.Error
    | ToggleDone Todo
    | ToggleEditTodo Todo
    | UpdateTodo Todo
    | DeleteTodo Todo
    | NoOp
