module Todos.State exposing (..)

import Return exposing (Return)
import Todos.Types exposing (..)
import Todo.Types as Todo


-- Model


initialVisibility : Visibility
initialVisibility =
    All


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

                    todoItem_ =
                        { todoItem
                            | editable = True
                            , description = todo.description
                        }
                in
                    Return.map <| updateTodoItem todoItem_

            CancelEditTodo todoItem ->
                let
                    todo_ =
                        .todo todoItem

                    todoItem_ =
                        { todoItem
                            | editable = False
                            , description = ""
                            , todo = { todo_ | description = .description todoItem }
                        }
                in
                    Return.map <| updateTodoItem todoItem_

            UpdateTodo todoItem ->
                let
                    todo_ =
                        todoItem.todo

                    todoItem_ =
                        { todoItem
                            | editable = False
                            , description = ""
                        }
                in
                    Return.map <| updateTodoItem todoItem_

            UpdateDescription todoItem description ->
                let
                    todo_ =
                        .todo todoItem

                    todoItem_ =
                        { todoItem
                            | todo = { todo_ | description = description }
                        }
                in
                    Return.map <| updateTodoItem todoItem_

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
        (\todoItem_ ->
            if todoItem_.todo.id == todoItem.todo.id then
                todoItem
            else
                todoItem_
        )


getTodoItem : TodoItem -> Todos -> Maybe TodoItem
getTodoItem todoItem todos =
    List.head <|
        List.filter
            (\todoItem_ ->
                todoItem_.todo.id == todoItem.todo.id
            )
            todos
