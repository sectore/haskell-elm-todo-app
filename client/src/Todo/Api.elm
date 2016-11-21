module Todo.Api
    exposing
        ( saveTodo
        , deleteTodo
        , updateTodo
        )

import Http
import Json.Encode as Encode
import Todo.Types exposing (..)
import Json.Decode as Decode exposing (Decoder)


todoEncoded : Todo -> Encode.Value
todoEncoded todo =
    let
        list =
            [ ( "completed", Encode.bool todo.completed )
            , ( "description", Encode.string todo.description )
            ]
    in
        list |> Encode.object


apiSaveTodo : Todo -> Http.Request Int
apiSaveTodo todo =
    let
        body =
            todoEncoded todo
                |> Encode.encode 0
                |> Http.stringBody "application/json"
    in
        Http.post "http://localhost:3000/todo/" body Decode.int


saveTodo : Todo -> Cmd Msg
saveTodo todo =
    apiSaveTodo todo
        |> Http.send Saved


apiDeleteTodo : Todo -> Http.Request String
apiDeleteTodo todo =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:3000/todo/" ++ toString todo.id
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }


deleteTodo : Todo -> Cmd Msg
deleteTodo todo =
    apiDeleteTodo todo
        |> Http.send Deleted


apiUpdateTodo : Todo -> Http.Request String
apiUpdateTodo todo =
    let
        body =
            todoEncoded todo
                |> Encode.encode 0
                |> Http.stringBody "application/json"
    in
        Http.request
            { method = "PUT"
            , headers = []
            , url = "http://localhost:3000/todo/" ++ toString todo.id
            , body = body
            , expect = Http.expectString
            , timeout = Nothing
            , withCredentials = False
            }


updateTodo : Todo -> Cmd Msg
updateTodo todo =
    apiUpdateTodo todo
        |> Http.send Updated
