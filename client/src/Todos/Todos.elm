module Todos.Todos exposing (..)

import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html exposing (..)
import Http
import Todo.Todo as Todo
import Task
import Api


type alias Model =
    { todos : List TodoItem
    }


type alias TodoItem =
    { todo : Todo.Model
    , editable : Bool
    }


initialModel : Model
initialModel =
    { todos = []
    }


type Msg
    = FetchTodosDone (List Todo.Model)
    | FetchTodosFail Http.Error
    | ToggleDone Todo.Model
    | ToggleEditTodo Todo.Model
    | UpdateTodo Todo.Model
    | DeleteTodo Todo.Model
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleDone todo ->
            let
                toggleDone todo =
                    { todo | completed = not todo.completed }

                todos =
                    List.map
                        (\todo' ->
                            if todo'.todo == todo then
                                { todo' | todo = toggleDone todo }
                            else
                                todo'
                        )
                        model.todos
            in
                -- TODO: call api to update
                ( { model | todos = todos }, Cmd.none )

        ToggleEditTodo todo ->
            let
                todos =
                    List.map
                        (\todo' ->
                            if todo'.todo == todo then
                                { todo' | editable = not todo'.editable }
                            else
                                todo'
                        )
                        model.todos
            in
                ( { model | todos = todos }, Cmd.none )

        UpdateTodo todo ->
            let
                todos =
                    List.map
                        (\todo' ->
                            if todo'.todo == todo then
                                { todo' | editable = False }
                            else
                                todo'
                        )
                        model.todos
            in
                -- TODO call Api
                ( { model | todos = todos }, Cmd.none )

        DeleteTodo todo ->
            ( model, Cmd.none )

        FetchTodosDone todos ->
            let
                todos' =
                    List.map (\todo -> TodoItem todo True) todos
            in
                { model | todos = todos' } ! []

        FetchTodosFail error ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


getTodos : Cmd Msg
getTodos =
    Api.getTodos
        |> Task.perform FetchTodosFail FetchTodosDone


listView : Model -> Html Msg
listView model =
    ul [ class "list-reset m0" ]
        (List.map todoView model.todos)


todoView : TodoItem -> Html Msg
todoView model =
    let
        todo =
            model.todo
    in
        li [ class "flex flex-center h1 p2 border-bottom gray" ]
            [ button
                [ class "h4 regular italic btn pl0"
                , onClick <| ToggleDone todo
                ]
                [ text <|
                    if todo.completed then
                        "Done "
                    else
                        "Todo"
                ]
            , div [ class "flex-auto" ]
                [ if model.editable then
                    input
                        [ class "block h1 col-12 border-rounded black muted"
                        , type' "text"
                        , value todo.description
                        ]
                        []
                  else
                    text todo.description
                ]
            , button
                [ class "h4 regular btn"
                , onClick <|
                    if model.editable then
                        ToggleEditTodo todo
                    else
                        DeleteTodo todo
                ]
                [ text <|
                    if model.editable then
                        "Cancel"
                    else
                        "Delete"
                ]
            , button
                [ class "h4 regular btn"
                , onClick <|
                    if model.editable then
                        UpdateTodo todo
                    else
                        ToggleEditTodo todo
                ]
                [ text <|
                    if model.editable then
                        "Update"
                    else
                        "Edit"
                ]
            ]
