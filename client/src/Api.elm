module Api exposing (getTodos, saveTodo)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Json.Decode.Pipeline as Pipeline
import Task
import Todo.Todo as Todo


getTodos : Task.Task Http.Error (List Todo.Model)
getTodos =
    Http.get todosDecoder "http://localhost:3000/todos/"


todoDecoder : Decoder Todo.Model
todoDecoder =
    Pipeline.decode Todo.Model
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "completed" Decode.bool
        |> Pipeline.required "description" Decode.string


todosDecoder : Decoder (List Todo.Model)
todosDecoder =
    Decode.list todoDecoder


todoEncoded : Todo.Model -> Encode.Value
todoEncoded todo =
    let
        list =
            [ ( "completed", Encode.bool todo.completed )
            , ( "description", Encode.string todo.description )
            ]
    in
        list |> Encode.object


saveTodo : Todo.Model -> Task.Task Http.Error Int
saveTodo todo =
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
