module Todos.Api exposing (getTodos)

import Http
import Json.Decode as Decode exposing (Decoder)
import Todo.Types as Todo
import Todos.Types exposing (..)


getTodos : Cmd Msg
getTodos =
    Http.get
        { url = "http://localhost:3000/todos/"
        , expect = Http.expectJson TodosFetched todosDecoder
        }


todoDecoder : Decoder Todo.Todo
todoDecoder =
    Decode.map3 Todo.Todo
        (Decode.field "id" Decode.int)
        (Decode.field "completed" Decode.bool)
        (Decode.field "description" Decode.string)


todosDecoder : Decoder (List Todo.Todo)
todosDecoder =
    Decode.list todoDecoder
