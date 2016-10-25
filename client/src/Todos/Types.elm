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
    = ToggleTodoDone Todo.Todo
    | ToggleTodoEdit Todo.Todo
    | UpdateTodoDescription Todo.Todo String
    | UpdateTodo Todo.Todo
    | DeleteTodo Todo.Todo
    | FetchTodosFail Http.Error
    | FetchTodosDone (List Todo.Todo)
