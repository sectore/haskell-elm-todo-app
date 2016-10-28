module Todos.Types exposing (..)

import Http
import Todo.Types as Todo


type alias TodoItem =
    { todo : Todo.Todo
    , description : String
    , editable : Bool
    }


type alias TodoItems =
    List TodoItem


type alias Todos =
    List TodoItem


type Visibility
    = All
    | Done
    | Active


type Msg
    = ToggleTodoDone TodoItem
    | EditTodo TodoItem
    | CancelEditTodo TodoItem
    | UpdateDescription TodoItem String
    | UpdateTodo TodoItem
    | DeleteTodo TodoItem
    | FetchTodosFail Http.Error
    | FetchTodosDone (List Todo.Todo)
    | SetVisibility Visibility
