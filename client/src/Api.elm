module Api exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Task
import Todo.Todo as Todo
import Todos.Todos as Todos exposing (Msg(..))


getTodos : Cmd Msg
getTodos =
    Http.get todosDecoder "http://localhost:3000/todos/"
        |> Task.perform Todos.FetchTodosFail Todos.FetchTodosDone


todoDecoder : Decoder Todo.Model
todoDecoder =
    Pipeline.decode Todo.Model
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "todoCompleted" Decode.bool
        |> Pipeline.required "todoDescription" Decode.string


todosDecoder : Decode.Decoder (List Todo.Model)
todosDecoder =
    Decode.list todoDecoder
