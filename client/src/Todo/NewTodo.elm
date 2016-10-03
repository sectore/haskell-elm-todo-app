module Todo.NewTodo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, keyCode, on, onClick)
import Json.Decode as Decode
import Api exposing (saveTodo)
import Todo.Todo as Todo
import Http
import Task


type alias Model =
    { todo : Todo.Model
    , request : Bool
    }


initialModel : Model
initialModel =
    { todo = Todo.emptyTodo
    , request = False
    }


type Msg
    = Input String
    | Enter
    | Cancel
    | SaveDone Int
    | SaveFail Http.Error
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input value ->
            let
                todo' =
                    model.todo
            in
                ( { model | todo = { todo' | description = value } }, Cmd.none )

        Enter ->
            ( { model | request = True }
            , saveTodo model.todo
            )

        Cancel ->
            ( { model | todo = Todo.emptyTodo }, Cmd.none )

        SaveDone id' ->
            ( { model
                | request = False
                , todo = Todo.emptyTodo
              }
            , Cmd.none
            )

        SaveFail error ->
            ( { model | request = False }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


saveTodo : Todo.Model -> Cmd Msg
saveTodo todo =
    Api.saveTodo todo
        |> Task.perform SaveFail SaveDone


view : Model -> Html Msg
view model =
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
