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


initialModel : Model
initialModel =
    { todos = Todos.initialTodos
    , newTodo = Todo.initialNewTodo
    }


initialCommand : Cmd Msg
initialCommand =
    Cmd.map TodosMsg Todos.getTodos


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

                        cmd' =
                            Cmd.map TodoMsg <| Todo.saveTodo todoModel
                    in
                        Return.mapWith
                            (\m -> { m | todos = todos, newTodo = Todo.emptyTodo })
                            cmd'

                _ ->
                    Return.mapWith (\m -> { m | newTodo = todoModel }) (Cmd.map TodoMsg todoCmd)


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
                    let
                        todos' =
                            List.map Todos.createTodoItem todos
                    in
                        Return.map (\m -> { m | todos = todos' })

                Todos.DeleteTodo todoItem ->
                    let
                        todos' =
                            Todos.deleteTodoItem todoItem todosModel

                        todoCmd =
                            Cmd.map TodoMsg <| (Todo.deleteTodo todoItem.todo)
                    in
                        Return.mapWith (\m -> { m | todos = todos' }) todoCmd

                Todos.SaveTodo todoItem ->
                    -- get a fresh todoItem again, which is just updated by sub module before
                    case Todos.getTodoItem todoItem todosModel of
                        Just todoItem' ->
                            Return.mapWith (\m -> { m | todos = todosModel }) (Cmd.map TodoMsg <| Todo.updateTodo todoItem'.todo)

                        Nothing ->
                            Return.zero

                Todos.ToggleTodoDone todoItem ->
                    let
                        todo =
                            todoItem.todo

                        todo' =
                            { todo | completed = not todo.completed }

                        todos' =
                            Todos.updateTodo todo' todosModel

                        todoCmd =
                            Cmd.map TodoMsg <| Todo.updateTodo todo'
                    in
                        Return.mapWith (\m -> { m | todos = todos' }) todoCmd

                _ ->
                    Return.mapWith (\m -> { m | todos = todosModel }) (Cmd.map TodosMsg todosCmd)
