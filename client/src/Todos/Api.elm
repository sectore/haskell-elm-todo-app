module Todos.Api exposing (getTodos, deleteTodo)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Task
import Todo.Types as T
import Todos.Types as Ts
import Todo.Api as T


getTodos : Cmd Ts.Msg
getTodos =
    apiGetTodos
        |> Task.perform Ts.FetchTodosFail Ts.FetchTodosDone


apiGetTodos : Task.Task Http.Error (List T.Todo)
apiGetTodos =
    Http.get todosDecoder "http://localhost:3000/todos/"


todoDecoder : Decoder T.Todo
todoDecoder =
    Pipeline.decode T.Todo
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "completed" Decode.bool
        |> Pipeline.required "description" Decode.string


todosDecoder : Decoder (List T.Todo)
todosDecoder =
    Decode.list todoDecoder


deleteTodo : T.Todo -> Cmd Ts.Msg
deleteTodo todo =
    T.apiDeleteTodo todo
        |> Task.perform Ts.DeleteTodoFail Ts.DeleteTodoDone
