module Todos.Todos exposing (..)

import Html exposing (..)
import Http
import Material.List as Lists
import Todo.Todo as Todo


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


listView : Model -> Html Msg
listView model =
    Lists.ul
        []
        (List.map itemView model.todos)


itemView : Todo.Model -> Html Msg
itemView todo =
    Lists.li []
        [ text todo.todoDescription ]
