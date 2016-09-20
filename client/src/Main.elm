module Main exposing (..)

import Api
import Html exposing (..)
import Html.App as Html
import Maybe
import Todo.NewTodo as NewTodo
import Todos.Todos as Todos


type alias Model =
    { todos : Todos.Model
    , newTodo : NewTodo.Model
    }


initialModel : Model
initialModel =
    { todos = Todos.initialModel
    , newTodo = NewTodo.initialModel
    }


type Msg
    = TodosMsg Todos.Msg
    | NewTodoMsg NewTodo.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TodosMsg msg' ->
            let
                ( updatedModel, cmd ) =
                    Todos.update msg' model.todos
            in
                ( { model | todos = updatedModel }, Cmd.map TodosMsg cmd )

        NewTodoMsg msg' ->
            let
                ( updatedModel, cmd ) =
                    NewTodo.update msg' model.newTodo
            in
                ( { model | newTodo = updatedModel }, Cmd.map NewTodoMsg cmd )


view : Model -> Html Msg
view model =
    div []
        [ Html.map NewTodoMsg (NewTodo.view model.newTodo)
        , h1 [] [ text <| Maybe.withDefault "" model.newTodo.description ]
        , Html.map TodosMsg (Todos.listView model.todos)
        ]


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.map TodosMsg Api.getTodos )


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
