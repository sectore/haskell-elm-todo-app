module Todos.View exposing (..)

import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html exposing (..)
import Todos.Types exposing (..)
import Todos.State exposing (..)


listView : Todos -> Html Msg
listView todos =
    ul [ class "list-reset m0" ]
        (List.map itemView todos)


itemView : TodoItem -> Html Msg
itemView item =
    let
        editable =
            itemEditableL.get item

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
                    if todoCompletedL.get item then
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
                    , value
                        (item
                            |> if editable then
                                .get itemDescriptionL
                               else
                                .get todoDescriptionL
                        )
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
