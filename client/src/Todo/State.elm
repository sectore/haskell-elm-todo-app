module Todo.State exposing (..)

import Return exposing (Return)
import Todo.Types exposing (..)



-- Model


emptyTodo : Todo
emptyTodo =
    Todo -1 False ""


initialNewTodo : Todo
initialNewTodo =
    emptyTodo



-- update


update : Msg -> Todo -> Return Msg Todo
update msg todo =
    Return.singleton todo
        |> (case msg of
                Update value ->
                    Return.map (\todo_ -> { todo_ | description = value })

                Cancel ->
                    Return.map <| always emptyTodo

                _ ->
                    Return.zero
           )
