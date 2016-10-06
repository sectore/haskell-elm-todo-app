module Todos.View exposing (..)

import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html exposing (..)
import Todos.Types exposing (..)


listView : Todos -> Html Msg
listView todos =
    ul [ class "list-reset m0" ]
        (List.map todoView todos)


todoView : TodoItem -> Html Msg
todoView item =
    let
        todo =
            item.todo
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
                [ if item.editable then
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
                    if item.editable then
                        ToggleEditTodo todo
                    else
                        DeleteTodo todo
                ]
                [ text <|
                    if item.editable then
                        "Cancel"
                    else
                        "Delete"
                ]
            , button
                [ class "h4 regular btn"
                , onClick <|
                    if item.editable then
                        UpdateTodo todo
                    else
                        ToggleEditTodo todo
                ]
                [ text <|
                    if item.editable then
                        "Update"
                    else
                        "Edit"
                ]
            ]
