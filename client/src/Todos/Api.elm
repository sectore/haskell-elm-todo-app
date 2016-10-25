module Todos.Api exposing (getTodos)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Task
import Todo.Types as Todo
import Todos.Types exposing (..)


apiGetTodos : Task.Task Http.Error (List Todo.Todo)
apiGetTodos =
    Http.get todosDecoder "http://localhost:3000/todos/"


getTodos : Cmd Msg
getTodos =
    apiGetTodos
        |> Task.perform FetchTodosFail FetchTodosDone


todoDecoder : Decoder Todo.Todo
todoDecoder =
    Pipeline.decode Todo.Todo
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "completed" Decode.bool
        |> Pipeline.required "description" Decode.string


todosDecoder : Decoder (List Todo.Todo)
todosDecoder =
    Decode.list todoDecoder
