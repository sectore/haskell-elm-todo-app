module Todos.State exposing (..)

import Monocle.Lens exposing (Lens, compose)
import Return exposing (Return)
import Todos.Types exposing (..)
import Todo.Types as Todo
import Todo.State as Todo


-- Model


initialTodos : Todos
initialTodos =
    []



-- Lens


itemDescriptionL : Lens TodoItem String
itemDescriptionL =
    Lens .description (\x m -> { m | description = x })


itemEditableL : Lens TodoItem Bool
itemEditableL =
    Lens .editable (\x m -> { m | editable = x })


itemTodoL : Lens TodoItem Todo.Todo
itemTodoL =
    Lens .todo (\x m -> { m | todo = x })


todoIdL : Lens TodoItem Int
todoIdL =
    itemTodoL `compose` Todo.idL


todoDescriptionL : Lens TodoItem String
todoDescriptionL =
    itemTodoL `compose` Todo.descriptionL


todoCompletedL : Lens TodoItem Bool
todoCompletedL =
    itemTodoL `compose` Todo.completedL



-- update


update : Msg -> Todos -> Return Msg Todos
update msg todos =
    Return.singleton todos
        |> case msg of
            EditTodo todoItem ->
                let
                    description =
                        todoItem
                            |> .get todoDescriptionL

                    todoItem' =
                        todoItem
                            |> .set itemEditableL True
                            |> .set itemDescriptionL description
                in
                    Return.map <| updateTodoItem todoItem'

            CancelEditTodo todoItem ->
                let
                    todoItem' =
                        todoItem
                            |> .set itemEditableL False
                            |> .set itemDescriptionL ""
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
            if .get todoIdL todoItem == .get Todo.idL todo then
                .set itemTodoL todo todoItem
            else
                todoItem
        )


updateTodoItem : TodoItem -> Todos -> Todos
updateTodoItem todoItem =
    List.map
        (\todoItem' ->
            if .get todoIdL todoItem' == .get todoIdL todoItem then
                todoItem
            else
                todoItem'
        )


getTodoItem : TodoItem -> Todos -> Maybe TodoItem
getTodoItem todoItem todos =
    List.head <|
        List.filter
            (\todoItem' ->
                .get todoIdL todoItem' == .get todoIdL todoItem
            )
            todos
