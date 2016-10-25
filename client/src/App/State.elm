module App.State exposing (..)

import App.Types exposing (..)
import Todos.State as Todos
import Todos.Api as Todos
import Todos.Types as Todos
import Todos.State as Todos
import Todo.State as Todo
import Todo.Api as Todo
import Todo.Types as Todo


initialModel : Model
initialModel =
    { todos = Todos.initialTodos
    , newTodo = Todo.initialNewTodo
    }


initialCommand : Cmd Msg
initialCommand =
    Cmd.map TodosMsg Todos.getTodos


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TodosMsg msg' ->
            updateTodos msg' model

        TodoMsg msg' ->
            updateTodo msg' model


updateTodos : Todos.Msg -> Model -> ( Model, Cmd Msg )
updateTodos msg model =
    let
        ( todos, cmd ) =
            Todos.update msg model.todos

        ( todos', cmd' ) =
            case msg of
                Todos.FetchTodosDone todos'' ->
                    ( List.map Todos.createTodoItem todos''
                    , Cmd.none
                    )

                Todos.FetchTodosFail error ->
                    ( todos, Cmd.none )

                Todos.DeleteTodo todoItem ->
                    ( Todos.deleteTodoItem todoItem todos
                    , Cmd.map TodoMsg <| Todo.deleteTodo todoItem.todo
                    )

                Todos.SaveTodo todoItem ->
                    -- get a fresh todoItem again, which is just updated by sub module before
                    case Todos.getTodoItem todoItem todos of
                        Just todoItem' ->
                            ( todos, Cmd.map TodoMsg <| Todo.updateTodo todoItem'.todo )

                        Nothing ->
                            ( todos, Cmd.none )

                Todos.ToggleTodoDone todoItem ->
                    let
                        todo =
                            todoItem.todo

                        todo' =
                            { todo | completed = not todo.completed }
                    in
                        ( Todos.updateTodo todo' todos
                        , Cmd.map TodoMsg <| Todo.updateTodo todo'
                        )

                _ ->
                    ( todos, Cmd.map TodosMsg cmd )
    in
        ( { model | todos = todos' }, cmd' )


updateTodo : Todo.Msg -> Model -> ( Model, Cmd Msg )
updateTodo msg model =
    let
        ( todo, cmd ) =
            Todo.update msg model.newTodo
    in
        case msg of
            Todo.Save ->
                ( { model
                    | todos = List.append model.todos [ Todos.createTodoItem todo ]
                    , newTodo = Todo.emptyTodo
                  }
                , Cmd.map TodoMsg <| Todo.saveTodo todo
                )

            _ ->
                ( { model | newTodo = todo }, Cmd.map TodoMsg cmd )
