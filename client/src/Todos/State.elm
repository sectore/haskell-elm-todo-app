module Todos.State exposing (..)

import Todos.Types exposing (..)
import Todo.Types as Todo


initialTodos : Todos
initialTodos =
    []


update : Msg -> Todos -> ( Todos, Cmd Msg )
update msg todos =
    case msg of
        EditTodo todoItem ->
            let
                todoItem' =
                    { todoItem
                        | editable = True
                        , description = todoItem.todo.description
                    }
            in
                ( updateTodoItem todoItem' todos, Cmd.none )

        CancelEditTodo todoItem ->
            let
                todoItem' =
                    { todoItem
                        | editable = False
                        , description = ""
                    }
            in
                ( updateTodoItem todoItem' todos, Cmd.none )

        SaveTodo todoItem ->
            let
                todo' =
                    todoItem.todo

                description =
                    todoItem.description

                todoItem' =
                    { todoItem
                        | editable = False
                        , description = ""
                        , todo = { todo' | description = description }
                    }
            in
                ( updateTodoItem todoItem' todos, Cmd.none )

        UpdateDescription todoItem description ->
            let
                todos' =
                    List.map
                        (\todoItem' ->
                            if todoItem' == todoItem then
                                { todoItem | description = description }
                            else
                                todoItem'
                        )
                        todos
            in
                ( todos', Cmd.none )

        _ ->
            ( todos, Cmd.none )



-- state helper functions


createTodoItem : Todo.Todo -> TodoItem
createTodoItem todo =
    TodoItem todo "" False


deleteTodoItem : TodoItem -> Todos -> Todos
deleteTodoItem todoItem =
    List.filter (\todoItem' -> todoItem /= todoItem')


updateTodo : Todo.Todo -> Todos -> Todos
updateTodo todo =
    List.map
        (\todo' ->
            if todo'.todo.id == todo.id then
                { todo' | todo = todo }
            else
                todo'
        )


updateTodoItem : TodoItem -> Todos -> Todos
updateTodoItem todoItem =
    List.map
        (\todoItem' ->
            if todoItem'.todo.id == todoItem.todo.id then
                todoItem
            else
                todoItem'
        )


getTodoItem : TodoItem -> Todos -> Maybe TodoItem
getTodoItem todoItem todos =
    List.head <|
        List.filter
            (\todoItem' ->
                todoItem'.todo.id == todoItem.todo.id
            )
            todos


toggleTodoDone : Todo.Todo -> Todos -> Todos
toggleTodoDone todo =
    List.map
        (\todoItem ->
            if todoItem.todo == todo then
                { todoItem | todo = { todo | completed = not todo.completed } }
            else
                todoItem
        )
