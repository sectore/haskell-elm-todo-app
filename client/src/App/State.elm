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

                Todos.DeleteTodo todo ->
                    ( Todos.deleteTodoItem todo todos
                    , Cmd.map TodoMsg <| Todo.deleteTodo todo
                    )

                Todos.UpdateTodo todo ->
                    case Todos.getTodoItem todo todos of
                        Nothing ->
                            ( todos, Cmd.none )

                        Just todoItem ->
                            ( todos, Cmd.map TodoMsg <| Todo.updateTodo todoItem.todo )

                Todos.ToggleTodoDone todo ->
                    let
                        todo' =
                            { todo | completed = not todo.completed }

                        todos' =
                            Todos.updateTodoItem todo' todos
                    in
                        ( todos', Cmd.map TodoMsg <| Todo.updateTodo todo' )

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
