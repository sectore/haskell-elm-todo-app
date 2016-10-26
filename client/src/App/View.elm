module App.View exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Todo.View as Todo
import Todos.View as Todos
import App.Types exposing (Model, Msg(..))
import App.State exposing (newTodoL, todosL)


root : Model -> Html Msg
root model =
    div []
        [ div
            [ class "center bg-black bg-cover bg-center header" ]
            [ div [ class "bg-darken-4 py4" ]
                [ Html.map TodoMsg <|
                    Todo.newTodo <|
                        newTodoL.get model
                ]
            ]
        , Html.map TodosMsg <|
            Todos.listView <|
                todosL.get model
        ]
