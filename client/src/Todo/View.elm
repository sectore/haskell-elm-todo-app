module Todo.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Decode
import String
import Todo.Types exposing (Msg(..), Todo)


newTodo : Todo -> Html Msg
newTodo todo =
    let
        hasEmptyDescription : Todo -> Bool
        hasEmptyDescription t =
            String.isEmpty <| .description t
    in
    div [ class "clearfix mt3 mb3" ]
        [ h1 [ class "2 regular caps silver" ]
            [ text "Haskell + Elm Todo List" ]
        , input
            [ class "col-10 field h2 p2 mt2 mb2 border-none navy"
            , type_ "text"
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
            ]
        , button
            [ class <|
                "btn  h5 regular silver underline"
                    ++ (if hasEmptyDescription todo then
                            " muted"

                        else
                            ""
                       )
            , onClick Cancel
            , disabled <| hasEmptyDescription todo
            ]
            [ text "Or skip" ]
        ]


onKeyDown : Attribute Msg
onKeyDown =
    let
        keyDecoder keyCode_ =
            case keyCode_ of
                13 ->
                    Save

                27 ->
                    Cancel

                _ ->
                    NoOp
    in
    on "keydown" <| Decode.map keyDecoder keyCode
