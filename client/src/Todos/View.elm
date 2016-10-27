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
    in
        li [ class "flex flex-center p2 border-bottom silver" ]
            [ div [ class "pr2" ]
                [ button
                    [ class <|
                        "h6 regular italic btn m0 p0 pl1 pr1 white rounded"
                            ++ if todoCompletedL.get item then
                                " bg-green "
                               else
                                " bg-gray"
                    , onClick <| ToggleTodoDone item
                    ]
                    [ text <|
                        if todoCompletedL.get item then
                            "Done "
                        else
                            "Todo"
                    ]
                ]
            , div
                [ class "flex-auto"
                ]
                [ input
                    [ class <|
                        "block h1 col-12 navy muted "
                            ++ if editable then
                                "border border-navy"
                               else
                                "muted border-none"
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
            , if editable then
                div []
                    [ button
                        [ class
                            "ml2 h4 regular btn btn-outline green"
                        , onClick <| SaveTodo item
                        ]
                        [ text "Update"
                        ]
                    , button
                        [ class <|
                            "h4 regular btn btn-outline ml2 gray"
                        , onClick <| CancelEditTodo item
                        ]
                        [ text "Cancel"
                        ]
                    ]
              else
                div []
                    [ button
                        [ class "ml2 h4 regular btn btn-outline fuchsia"
                        , onClick <| DeleteTodo item
                        ]
                        [ text "Delete"
                        ]
                    , button
                        [ class <|
                            "h4 regular btn btn-outline ml2 navy"
                        , onClick <| EditTodo item
                        ]
                        [ text "Edit"
                        ]
                    ]
            ]
