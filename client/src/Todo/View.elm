module Todo.View exposing (..)

import Json.Decode as Decode
import Todo.Types exposing (NewTodo, Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, keyCode, on, onClick)


newTodo : NewTodo -> Html Msg
newTodo model =
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
                ]
                [ text "Add Todo" ]
            , svg [ class "icon gray", attribute "data-icon" "chevron-right" ] []
            ]
        , button
            [ class "btn silver h5 regular underline"
            , onClick Cancel
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
