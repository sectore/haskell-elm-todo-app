module Todo.Api exposing (saveTodo, apiDeleteTodo)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task
import Todo.Types exposing (..)
import Http.Decorators


todoEncoded : Todo -> Encode.Value
todoEncoded todo =
    let
        list =
            [ ( "completed", Encode.bool todo.completed )
            , ( "description", Encode.string todo.description )
            ]
    in
        list |> Encode.object


apiSaveTodo : Todo -> Task.Task Http.Error Int
apiSaveTodo todo =
    let
        body =
            todoEncoded todo
                |> Encode.encode 0
                |> Http.string

        config =
            { verb = "POST"
            , headers = [ ( "Content-Type", "application/json" ) ]
            , url = "http://localhost:3000/todo/"
            , body = body
            }
    in
        Http.send Http.defaultSettings config
            |> Http.fromJson Decode.int


saveTodo : Todo -> Cmd Msg
saveTodo todo =
    apiSaveTodo todo
        |> Task.perform SaveFail SaveDone


apiDeleteTodo : Todo -> Task.Task Http.Error Http.Response
apiDeleteTodo todo =
    let
        config =
            { verb = "DELETE"
            , headers = []
            , url = "http://localhost:3000/todo/" ++ toString todo.id
            , body = Http.empty
            }
    in
        Http.send Http.defaultSettings config
            |> Http.Decorators.interpretStatus
