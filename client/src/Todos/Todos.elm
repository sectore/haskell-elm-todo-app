module Todos.Todos exposing (..)

import Html.Attributes exposing (..)
import Html exposing (..)
import Http
import Todo.Todo as Todo
import Task
import Api


type alias Model =
    { todos : List Todo.Model
    , selectedTodo : Maybe Todo.Model
    }


initialModel : Model
initialModel =
    { todos = []
    , selectedTodo = Nothing
    }


type Msg
    = FetchTodosDone (List Todo.Model)
    | FetchTodosFail Http.Error
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchTodosDone todos ->
            { model | todos = todos } ! []

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
        (List.map itemView model.todos)


itemView : Todo.Model -> Html Msg
itemView todo =
    li [ class "flex flex-center h1 p2 border-bottom gray" ]
        [ button [ class "h4 regular italic btn pl0" ] [ text "Done" ]
        , div [ class "flex-auto" ]
            [ text todo.description
            ]
        , button [ class "h4 regular btn" ] [ text "Edit" ]
        , button [ class "h4 regular btn" ] [ text "Delete" ]
        ]
