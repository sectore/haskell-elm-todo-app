module Todos.State exposing (..)

import Todos.Types exposing (..)
import Todo.Types as Todo


initialTodos : Todos
initialTodos =
    []


update : Msg -> Todos -> ( Todos, Cmd Msg )
update msg todos =
    case msg of
        ToggleTodoEdit todo ->
            ( toggleEditable todo todos, Cmd.none )

        UpdateTodo todo ->
            ( toggleEditable todo todos, Cmd.none )

        UpdateTodoDescription todo description ->
            let
                todos' =
                    List.map
                        (\todoItem ->
                            if todoItem.todo == todo then
                                { todoItem | description = description }
                            else
                                todoItem
                        )
                        todos
            in
                ( todos', Cmd.none )

        _ ->
            ( todos, Cmd.none )



-- state helper functions


toggleEditable : Todo.Todo -> Todos -> Todos
toggleEditable todo =
    List.map
        (\todoItem ->
            if todoItem.todo == todo then
                let
                    editable =
                        not todoItem.editable

                    itemDescription =
                        if editable then
                            todo.description
                        else
                            ""

                    todoDescription =
                        if editable then
                            todo.description
                        else
                            todoItem.description
                in
                    { todoItem
                        | editable = editable
                        , description = itemDescription
                        , todo = { todo | description = todoDescription }
                    }
            else
                todoItem
        )


createTodoItem : Todo.Todo -> TodoItem
createTodoItem todo =
    TodoItem todo "" False


deleteTodoItem : Todo.Todo -> Todos -> Todos
deleteTodoItem todo todos =
    List.filter (\todoItem -> todoItem.todo /= todo) todos


updateTodoItem : Todo.Todo -> Todos -> Todos
updateTodoItem todo =
    List.map
        (\todo' ->
            if todo'.todo.id == todo.id then
                { todo' | todo = todo }
            else
                todo'
        )


getTodoItem : Todo.Todo -> Todos -> Maybe TodoItem
getTodoItem todo todos =
    List.head <|
        List.filter
            (\todoItem ->
                todoItem.todo.id == todo.id
            )
            todos


toggleTodoDone : Todo.Todo -> Todos -> Todos
toggleTodoDone todo todos =
    List.map
        (\todoItem ->
            if todoItem.todo == todo then
                { todoItem | todo = { todo | completed = not todo.completed } }
            else
                todoItem
        )
        todos
