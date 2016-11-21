module App.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Todo.View as Todo
import Todos.View as Todos
import Todos.Types as Todos
import App.Types exposing (Model, Msg(..))


root : Model -> Html Msg
root model =
    div []
        [ header model
        , visibilityMenu model.todos model.todosVisibility
        , Html.map TodosMsg <| Todos.listView model.todos model.todosVisibility
        ]


header : Model -> Html Msg
header model =
    div [ class "center bg-black bg-cover bg-center header" ]
        [ div [ class "bg-darken-4 py4" ]
            [ Html.map TodoMsg <| Todo.newTodo <| .newTodo model
            , resultView <| .todos model
            ]
        ]


resultView : Todos.Todos -> Html Msg
resultView todos =
    let
        todosLength =
            List.length todos

        todosCompleted =
            List.length <|
                List.filter (\item -> .completed <| .todo <| item) todos
    in
        p
            [ class <|
                "h6 p1 green muted inline-block "
                    ++ if todosLength == 0 then
                        "hide"
                       else
                        ""
            ]
            [ text <| toString todosCompleted ++ " / " ++ toString todosLength ++ " done"
            ]


visibilityMenu : Todos.Todos -> Todos.Visibility -> Html Msg
visibilityMenu todos visibility =
    let
        isDisabled =
            List.length todos == 0

        activeClass visibility value =
            if visibility == value then
                " bg-silver gray "
            else
                ""

        menuBtnClass =
            "flex-auto h4 py2 regular btn btn-primary bg-gray white not-rounded "
    in
        div
            [ class "flex center "
            ]
            [ button
                [ class <| menuBtnClass ++ activeClass visibility Todos.All
                , onClick <| TodosMsg <| Todos.SetVisibility Todos.All
                , disabled isDisabled
                ]
                [ text "All"
                ]
            , button
                [ class <| menuBtnClass ++ activeClass visibility Todos.Done
                , onClick <| TodosMsg <| Todos.SetVisibility Todos.Done
                , disabled isDisabled
                ]
                [ text "Done"
                ]
            , button
                [ class <| menuBtnClass ++ activeClass visibility Todos.Active
                , onClick <| TodosMsg <| Todos.SetVisibility Todos.Active
                , disabled isDisabled
                ]
                [ text "Active"
                ]
            ]
