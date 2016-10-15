module Todos.State exposing (..)

import Todos.Types exposing (..)


initialTodos : Todos
initialTodos =
    []


update : Msg -> Todos -> ( Todos, Cmd Msg )
update msg todos =
    case msg of
        ToggleDone todo ->
            let
                todos' =
                    List.map
                        (\todo' ->
                            if todo'.todo == todo then
                                { todo' | todo = { todo | completed = not todo.completed } }
                            else
                                todo'
                        )
                        todos
            in
                -- TODO: call api to update todo
                ( todos', Cmd.none )

        ToggleEditTodo todo ->
            let
                todos' =
                    List.map
                        (\todo' ->
                            if todo'.todo == todo then
                                { todo' | editable = not todo'.editable }
                            else
                                todo'
                        )
                        todos
            in
                ( todos', Cmd.none )

        UpdateTodo todo ->
            let
                todos' =
                    List.map
                        (\todo' ->
                            if todo'.todo == todo then
                                { todo' | editable = False }
                            else
                                todo'
                        )
                        todos
            in
                -- TODO call Api
                ( todos', Cmd.none )

        DeleteTodo todo ->
            ( todos, Cmd.none )

        FetchTodosDone todos ->
            let
                todos' =
                    List.map (\todo -> TodoItem todo False) todos
            in
                todos' ! []

        FetchTodosFail error ->
            ( todos, Cmd.none )

        NoOp ->
            ( todos, Cmd.none )
