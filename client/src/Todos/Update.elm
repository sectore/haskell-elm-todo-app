module Todos.Update exposing (Msg(..), update)

import Http
import Todo.Model as Todo
import Todos.Model as Todos


type Msg
    = FetchTodosDone (List Todo.Model)
    | FetchTodosFail Http.Error
    | NoOp


update : Msg -> Todos.Model -> ( Todos.Model, Cmd Msg )
update msg model =
    case msg of
        FetchTodosDone todos ->
            { model | todos = todos } ! []

        FetchTodosFail error ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )
