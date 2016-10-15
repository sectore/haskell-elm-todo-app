module Todo.View exposing (..)

import Json.Decode as Decode
import Todo.Types exposing (NewTodo, Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, keyCode, on, onClick)
import String


newTodo : NewTodo -> Html Msg
newTodo model =
    let
        todo' =
            model.todo

        hasEmptyDescription =
            String.isEmpty todo'.description

        extraCancelButtonStyle =
            if hasEmptyDescription then
                " muted"
            else
                ""
    in
        div [ class "clearfix mt3 mb3" ]
            [ h1 [ class "2 regular caps silver" ]
                [ text "Haskell + Elm Todo List" ]
            , input
                [ class "col-10 field h2 p2 mt2 mb2 border-none"
                , type' "text"
                , value model.todo.description
                , placeholder "Enter new Todo"
                , onInput Input
                , autofocus True
                , onKeyDown
                ]
                []
            , div [ class "" ]
                [ button
                    [ class "h3 px4 py2 btn btn-outline lime"
                    , onClick Enter
                    , disabled hasEmptyDescription
                    ]
                    [ text "Add Todo" ]
                , svg [ class "icon gray", attribute "data-icon" "chevron-right" ] []
                ]
            , button
                [ class <| "btn  h5 regular silver underline" ++ extraCancelButtonStyle
                , onClick Cancel
                , disabled hasEmptyDescription
                ]
                [ text "Or skip" ]
            ]


onKeyDown : Attribute Msg
onKeyDown =
    let
        keyDecoder keyCode' =
            case keyCode' of
                13 ->
                    Enter

                27 ->
                    Cancel

                _ ->
                    NoOp
    in
        on "keydown" (Decode.map keyDecoder keyCode)
