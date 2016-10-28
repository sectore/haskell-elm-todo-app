module App.State exposing (..)

import App.Types exposing (..)
import Todos.State as Todos
import Todos.Api as Todos
import Todos.Types as Todos
import Todos.State as Todos
import Todo.State as Todo
import Todo.Api as Todo
import Todo.Types as Todo
import Return exposing (Return)


-- Model


initialModel : Model
initialModel =
    { todos = Todos.initialTodos
    , todosVisibility = Todos.initialVisibility
    , newTodo = Todo.initialNewTodo
    }



-- Cmd


initialCommand : Cmd Msg
initialCommand =
    Cmd.map TodosMsg Todos.getTodos



-- update


update : Msg -> Model -> Return Msg Model
update msg model =
    Return.singleton model
        |> case msg of
            TodosMsg msg' ->
                updateTodos msg'

            TodoMsg msg' ->
                updateTodo msg'


updateTodo : Todo.Msg -> Return Msg Model -> Return Msg Model
updateTodo msg writer =
    let
        ( appModel, _ ) =
            writer

        ( todoModel, todoCmd ) =
            Todo.update msg appModel.newTodo
    in
        writer
            |> case msg of
                Todo.Save ->
                    let
                        todos =
                            List.append appModel.todos [ Todos.createTodoItem todoModel ]
                    in
                        Return.mapWith
                            (\m -> { m | todos = todos, newTodo = Todo.emptyTodo })
                            (Cmd.map TodoMsg <|
                                Todo.saveTodo todoModel
                            )

                _ ->
                    Return.mapWith (\m -> { m | newTodo = todoModel }) <|
                        Cmd.map TodoMsg todoCmd


updateTodos : Todos.Msg -> Return Msg Model -> Return Msg Model
updateTodos msg writer =
    let
        ( appModel, _ ) =
            writer

        ( todosModel, todosCmd ) =
            Todos.update msg appModel.todos
    in
        writer
            |> case msg of
                Todos.FetchTodosDone todos ->
                    Return.map <|
                        (\m -> { m | todos = List.map Todos.createTodoItem todos })

                Todos.DeleteTodo todoItem ->
                    Return.mapWith
                        (\m -> { m | todos = Todos.deleteTodoItem todoItem todosModel })
                        (Cmd.map TodoMsg <|
                            Todo.deleteTodo todoItem.todo
                        )

                Todos.SaveTodo todoItem ->
                    -- FIXME: get a fresh todoItem again, which is just updated by sub module before
                    case Todos.getTodoItem todoItem todosModel of
                        Just todoItem' ->
                            Return.mapWith
                                (\m -> { m | todos = todosModel })
                                (Cmd.map TodoMsg <|
                                    Todo.updateTodo todoItem'.todo
                                )

                        Nothing ->
                            Return.zero

                Todos.ToggleTodoDone todoItem ->
                    let
                        todo =
                            todoItem.todo

                        todo' =
                            { todo | completed = not todo.completed }
                    in
                        Return.mapWith
                            (\m -> { m | todos = Todos.updateTodo todo' todosModel })
                            (Cmd.map TodoMsg <|
                                Todo.updateTodo todo'
                            )

                Todos.SetVisibility visibility ->
                    Return.map (\m -> { m | todosVisibility = visibility })

                _ ->
                    Return.mapWith
                        (\m -> { m | todos = todosModel })
                        (Cmd.map TodosMsg todosCmd)
