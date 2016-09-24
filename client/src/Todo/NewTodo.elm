module Todo.NewTodo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, keyCode, on)
import Debug exposing (log)
import Json.Decode as Decode
import Api exposing (saveTodo)
import Todo.Todo as Todo
import Http
import Task

type alias Model =
    { todo : Todo.Model
    , request : Bool
    }


emptyTodo : Todo.Model
emptyTodo =
    Todo.Model -1 False ""


initialModel : Model
initialModel =
    { todo = emptyTodo
    , request = False
    }


type Msg
    = Input String
    | Enter
    | Cancel
    | SaveTodoDone Int
    | SaveTodoFail Http.Error
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
            Debug.log "enter"
                ( { model | request = True }
                , saveTodo model.todo
                )

        Cancel ->
            Debug.log "cancel"
                ( { model | todo = emptyTodo }, Cmd.none )

        SaveTodoDone id' ->
            let
                todo' =
                    model.todo
            in
                ( { model
                    | request = False
                    , todo = { todo' | id = id' }
                  }
                , Cmd.none
                )

        SaveTodoFail error ->
            ( { model | request = False }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


saveTodo : Todo.Model -> Cmd Msg
saveTodo todo =
    Api.saveTodo todo
        |> Task.perform SaveTodoFail SaveTodoDone


view : Model -> Html Msg
view model =
    input
        [ placeholder "Enter new Todo"
        , value model.todo.description
        , onInput Input
        , autofocus True
        , onKeyDown
        ]
        []


onKeyDown : Attribute Msg
onKeyDown =
    let
        decoder keyCode' =
            case keyCode' of
                13 ->
                    Enter

                27 ->
                    Cancel

                _ ->
                    NoOp
    in
        on "keydown" (Decode.map decoder keyCode)
