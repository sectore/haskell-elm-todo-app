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

        editable =
            item.editable

        editableInputStyle =
            if editable then
                "border-rounded"
            else
                "border-none"
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
            , div
                [ class "flex-auto"
                , onClick <|
                    if not editable then
                        ToggleEditTodo todo
                    else
                        NoOp
                ]
                [ input
                    [ class <| "block h1 col-12 black muted " ++ editableInputStyle
                    , type' "text"
                    , disabled <| not editable
                    , value todo.description
                    ]
                    []
                ]
            , button
                [ class "h4 regular btn"
                , onClick <| ToggleEditTodo todo
                ]
                [ text <|
                    if editable then
                        "Cancel"
                    else
                        "Edit"
                ]
            , button
                [ class "h4 regular btn"
                , onClick <|
                    if editable then
                        UpdateTodo todo
                    else
                        DeleteTodo todo
                ]
                [ text <|
                    if editable then
                        "Update"
                    else
                        "Delete"
                ]
            ]
