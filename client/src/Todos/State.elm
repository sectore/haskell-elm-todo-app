module Todos.State exposing (..)

import Return exposing (Return)
import Todos.Types exposing (..)
import Todo.Types as Todo


initialTodos : Todos
initialTodos =
    []


update : Msg -> Todos -> Return Msg Todos
update msg todos =
    Return.singleton todos
        |> case msg of
            EditTodo todoItem ->
                let
                    todo =
                        todoItem.todo

                    todoItem' =
                        { todoItem
                            | editable = True
                            , description = todo.description
                        }
                in
                    Return.map (\todos' -> updateTodoItem todoItem' todos')

            CancelEditTodo todoItem ->
                let
                    todoItem' =
                        { todoItem
                            | editable = False
                            , description = ""
                        }
                in
                    Return.map (\todos' -> updateTodoItem todoItem' todos')

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
                    Return.map (\todos' -> updateTodoItem todoItem' todos')

            UpdateDescription todoItem description ->
                let
                    todoItem' =
                        { todoItem | description = description }
                in
                    Return.map (\todos' -> updateTodoItem todoItem' todos')

            _ ->
                Return.zero



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
