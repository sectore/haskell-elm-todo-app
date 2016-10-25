module Todos.View exposing (..)

import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html exposing (..)
import Todos.Types exposing (..)


listView : Todos -> Html Msg
listView todos =
    ul [ class "list-reset m0" ]
        (List.map itemView todos)


itemView : TodoItem -> Html Msg
itemView item =
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
                , onClick <| ToggleTodoDone item
                ]
                [ text <|
                    if todo.completed then
                        "Done "
                    else
                        "Todo"
                ]
            , div
                [ class "flex-auto"
                ]
                [ input
                    [ class <| "block h1 col-12 black muted " ++ editableInputStyle
                    , type' "text"
                    , disabled <| not editable
                    , value <|
                        if editable then
                            item.description
                        else
                            todo.description
                    , onInput (UpdateDescription item)
                    ]
                    []
                ]
            , button
                [ class "h4 regular btn"
                , onClick <|
                    if editable then
                        CancelEditTodo item
                    else
                        EditTodo item
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
                        SaveTodo item
                    else
                        DeleteTodo item
                ]
                [ text <|
                    if editable then
                        "Update"
                    else
                        "Delete"
                ]
            ]
