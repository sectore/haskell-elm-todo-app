module Todos.Api exposing (get, decoder)

import Http
import Json.Decode as Decode exposing (Decoder)
import Task
import Todo.Api as Todo
import Todo.Model as Todo
import Todos.Update as Todos exposing (Msg(..))


get : Cmd Msg
get =
    Http.get decoder "http://localhost:3000/todos/"
        |> Task.perform Todos.FetchTodosFail Todos.FetchTodosDone


decoder : Decode.Decoder (List Todo.Model)
decoder =
    Decode.list Todo.decoder
