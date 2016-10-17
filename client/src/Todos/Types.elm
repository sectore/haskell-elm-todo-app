module Todos.Types exposing (..)

import Http
import Todo.Types exposing (..)


type alias TodoItem =
    { todo : Todo
    , description : String
    , editable : Bool
    }


type alias Todos =
    List TodoItem


type Msg
    = FetchTodosDone (List Todo)
    | FetchTodosFail Http.Error
    | ToggleTodoDone Todo
    | ToggleTodoEdit Todo
    | UpdateTodoDescription Todo String
    | UpdateTodo Todo
    | UpdateTodoDone Http.Response
    | UpdateTodoFail Http.Error
    | DeleteTodo Todo
    | DeleteTodoDone Http.Response
    | DeleteTodoFail Http.Error
    | NoOp
