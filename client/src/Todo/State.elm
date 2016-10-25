module Todo.State exposing (..)

import Todo.Types exposing (..)


emptyTodo : Todo
emptyTodo =
    Todo -1 False ""


initialNewTodo : Todo
initialNewTodo =
    emptyTodo


update : Msg -> Todo -> ( Todo, Cmd Msg )
update msg todo =
    case msg of
        Update value ->
            ( { todo | description = value }, Cmd.none )

        Save ->
            ( todo, Cmd.none )

        Cancel ->
            ( emptyTodo, Cmd.none )

        NoOp ->
            ( todo, Cmd.none )

        _ ->
            ( todo, Cmd.none )
