module Todo.State exposing (..)

import Todo.Api exposing (saveTodo)
import Todo.Types exposing (..)


emptyTodo : Todo
emptyTodo =
    Todo -1 False ""


initialNewTodo : NewTodo
initialNewTodo =
    { todo = emptyTodo
    , request = False
    }


update : Msg -> NewTodo -> ( NewTodo, Cmd Msg )
update msg model =
    case msg of
        Input value ->
            let
                todo' =
                    model.todo
            in
                ( { model | todo = { todo' | description = value } }, Cmd.none )

        Enter ->
            ( { model | request = True }, saveTodo model.todo )

        Cancel ->
            ( { model | todo = emptyTodo }, Cmd.none )

        SaveDone id' ->
            ( { model
                | request = False
                , todo = emptyTodo
              }
            , Cmd.none
            )

        SaveFail error ->
            ( { model | request = False }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )
