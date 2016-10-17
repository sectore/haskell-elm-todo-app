module Todos.State exposing (..)

import Todos.Types exposing (..)
import Todo.Types as T
import Todos.Api exposing (deleteTodo, updateTodo)


initialTodos : Todos
initialTodos =
    []


updateEditableTodo : T.Todo -> Todos -> Todos
updateEditableTodo todo =
    List.map
        (\todoItem ->
            if todoItem.todo == todo then
                let
                    editable =
                        not todoItem.editable

                    description =
                        if editable then
                            todo.description
                        else
                            ""
                in
                    { todoItem | editable = not todoItem.editable, description = description }
            else
                todoItem
        )


updateTodoItem : T.Todo -> Todos -> Todos
updateTodoItem todo =
    List.map
        (\todo' ->
            if todo'.todo.id == todo.id then
                { todo' | todo = todo }
            else
                todo'
        )


getTodoItem : T.Todo -> Todos -> Maybe TodoItem
getTodoItem todo todos =
    List.head <|
        List.filter
            (\todo' ->
                todo'.todo.id == todo.id
            )
            todos


update : Msg -> Todos -> ( Todos, Cmd Msg )
update msg todos =
    case msg of
        ToggleTodoDone todo ->
            let
                todo' =
                    { todo | completed = not todo.completed }

                todos' =
                    List.map
                        (\todoItem ->
                            if todoItem.todo == todo then
                                { todoItem | todo = todo' }
                            else
                                todoItem
                        )
                        todos
            in
                ( todos', updateTodo todo' )

        ToggleTodoEdit todo ->
            ( updateEditableTodo todo todos, Cmd.none )

        UpdateTodoDescription todo description ->
            let
                todos' =
                    List.map
                        (\todo' ->
                            if todo'.todo == todo then
                                { todo' | description = description }
                            else
                                todo'
                        )
                        todos
            in
                ( todos', Cmd.none )

        UpdateTodo todo ->
            case getTodoItem todo todos of
                Nothing ->
                    ( todos, Cmd.none )

                Just todoItem ->
                    ( todos, updateTodo { todo | description = todoItem.description } )

        UpdateTodoDone _ ->
            ( todos, Cmd.none )

        UpdateTodoFail error ->
            ( todos, Cmd.none )

        DeleteTodo todo ->
            ( todos, deleteTodo todo )

        DeleteTodoDone _ ->
            ( todos, Cmd.none )

        DeleteTodoFail error ->
            ( todos, Cmd.none )

        FetchTodosDone todos ->
            let
                todos' =
                    List.map (\todo -> TodoItem todo "" False) todos
            in
                todos' ! []

        FetchTodosFail error ->
            ( todos, Cmd.none )

        NoOp ->
            ( todos, Cmd.none )
