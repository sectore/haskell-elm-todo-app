module App.State exposing (..)

import Todos.State as Ts
import App.Types exposing (..)
import Todos.Api as Ts
import Todos.Types as Ts
import Todo.State as T
import Todo.Types as T
import Todos.State as Ts


initialModel : Model
initialModel =
    { todos = Ts.initialTodos
    , newTodo = T.initialNewTodo
    }


initialCommand : Cmd Msg
initialCommand =
    Cmd.map TodosMsg Ts.getTodos


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TodosMsg msg' ->
            let
                ( updatedModel, cmd' ) =
                    Ts.update msg' model.todos

                cmd =
                    case msg' of
                        Ts.DeleteTodoDone _ ->
                            Cmd.map TodosMsg Ts.getTodos

                        _ ->
                            Cmd.map TodosMsg cmd'
            in
                ( { model | todos = updatedModel }, cmd )

        TodoMsg msg' ->
            let
                ( newTodoModel, cmd' ) =
                    T.update msg' model.newTodo

                cmd =
                    case msg' of
                        T.SaveDone _ ->
                            Cmd.map TodosMsg Ts.getTodos

                        _ ->
                            Cmd.map TodoMsg cmd'
            in
                ( { model | newTodo = newTodoModel }, cmd )
