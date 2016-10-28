module Todos.State exposing (..)

import Return exposing (Return)
import Todos.Types exposing (..)
import Todo.Types as Todo


-- Model


initialVisibility : Visibility
initialVisibility =
    Done


initialTodos : Todos
initialTodos =
    []



-- update


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
                    Return.map <| updateTodoItem todoItem'

            CancelEditTodo todoItem ->
                let
                    todoItem' =
                        { todoItem
                            | editable = False
                            , description = ""
                        }
                in
                    Return.map <| updateTodoItem todoItem'

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
                    Return.map <| updateTodoItem todoItem'

            UpdateDescription todoItem description ->
                let
                    todoItem' =
                        { todoItem | description = description }
                in
                    Return.map <| updateTodoItem todoItem'

            _ ->
                Return.zero



-- state helper functions


createTodoItem : Todo.Todo -> TodoItem
createTodoItem todo =
    TodoItem todo "" False


deleteTodoItem : TodoItem -> Todos -> Todos
deleteTodoItem todoItem =
    List.filter <| (/=) todoItem


updateTodo : Todo.Todo -> Todos -> Todos
updateTodo todo =
    List.map
        (\todoItem ->
            if todoItem.todo.id == todo.id then
                { todoItem | todo = todo }
            else
                todoItem
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
