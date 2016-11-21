module Todos.Api
    exposing
        ( getTodos
        )

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Todo.Types as Todo
import Todos.Types exposing (..)


apiGetTodos : Http.Request (List Todo.Todo)
apiGetTodos =
    Http.get "http://localhost:3000/todos/" todosDecoder


getTodos : Cmd Msg
getTodos =
    apiGetTodos
        |> Http.send TodosFetched


todoDecoder : Decoder Todo.Todo
todoDecoder =
    Pipeline.decode Todo.Todo
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "completed" Decode.bool
        |> Pipeline.required "description" Decode.string


todosDecoder : Decoder (List Todo.Todo)
todosDecoder =
    Decode.list todoDecoder
