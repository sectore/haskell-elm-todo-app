module Todo.View exposing (..)

import Json.Decode as Decode
import Todo.Types exposing (Todo, Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, keyCode, on, onClick)
import String


newTodo : Todo -> Html Msg
newTodo todo =
    let
        hasEmptyDescription : Todo -> Bool
        hasEmptyDescription todo =
            String.isEmpty <| .description todo
    in
        div [ class "clearfix mt3 mb3" ]
            [ h1 [ class "2 regular caps silver" ]
                [ text "Haskell + Elm Todo List" ]
            , input
                [ class "col-10 field h2 p2 mt2 mb2 border-none navy"
                , type' "text"
                , value todo.description
                , placeholder "Enter new Todo"
                , onInput Update
                , autofocus True
                , onKeyDown
                ]
                []
            , div [ class "" ]
                [ button
                    [ class "h3 px4 py2 btn btn-outline lime"
                    , onClick Save
                    , disabled <| hasEmptyDescription todo
                    ]
                    [ text "Add Todo" ]
                , svg [ class "icon gray", attribute "data-icon" "chevron-right" ] []
                ]
            , button
                [ class <|
                    "btn  h5 regular silver underline"
                        ++ if hasEmptyDescription todo then
                            " muted"
                           else
                            ""
                , onClick Cancel
                , disabled <| hasEmptyDescription todo
                ]
                [ text "Or skip" ]
            ]


onKeyDown : Attribute Msg
onKeyDown =
    let
        keyDecoder keyCode' =
            case keyCode' of
                13 ->
                    Save

                27 ->
                    Cancel

                _ ->
                    NoOp
    in
        on "keydown" <| Decode.map keyDecoder keyCode
