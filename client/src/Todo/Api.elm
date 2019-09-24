module Todo.Api exposing
    ( deleteTodo
    , saveTodo
    , updateTodo
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Todo.Types exposing (..)


todoEncoded : Todo -> Encode.Value
todoEncoded todo =
    let
        list =
            [ ( "completed", Encode.bool todo.completed )
            , ( "description", Encode.string todo.description )
            ]
    in
    list |> Encode.object


saveTodo : Todo -> Cmd Msg
saveTodo todo =
    let
        body =
            todoEncoded todo
                |> Encode.encode 0
                |> Http.stringBody "application/json"
    in
    Http.post
        { url = "http://localhost:3000/todo/"
        , body = body
        , expect = Http.expectJson Saved Decode.int
        }


deleteTodo : Todo -> Cmd Msg
deleteTodo todo =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:3000/todo/" ++ String.fromInt todo.id
        , body = Http.emptyBody
        , expect = Http.expectString Deleted
        , timeout = Nothing
        , tracker = Nothing
        }


updateTodo : Todo -> Cmd Msg
updateTodo todo =
    let
        body =
            todoEncoded todo
                |> Encode.encode 0
                |> Http.stringBody "application/json"
    in
    Http.request
        { method = "PUT"
        , headers = []
        , url = "http://localhost:3000/todo/" ++ String.fromInt todo.id
        , body = body
        , timeout = Nothing
        , expect = Http.expectString Updated
        , tracker = Nothing
        }
