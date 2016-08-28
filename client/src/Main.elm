module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Http
import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Decode.Pipeline as Pipeline
import Task


-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { todos : List Todo
    , selectedTodo : Maybe Todo
    }


type alias Todo =
    { id : Int
    , todoCompleted : Bool
    , todoDescription : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] Nothing, getTodos )



-- UPDATE


type Msg
    = FetchTodosDone (List Todo)
    | FetchTodosFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchTodosDone todos ->
            { model | todos = todos } ! []

        FetchTodosFail error ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getTodos : Cmd Msg
getTodos =
    Http.get todosDecoder "http://localhost:3000/todos/"
        |> Task.perform FetchTodosFail FetchTodosDone


todosDecoder : Decode.Decoder (List Todo)
todosDecoder =
    Decode.list todoDecoder


todoDecoder : Decoder Todo
todoDecoder =
    Pipeline.decode Todo
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "todoCompleted" Decode.bool
        |> Pipeline.required "todoDescription" Decode.string



-- VIEW


todoView : Todo -> Html Msg
todoView todo =
    li []
        [ text todo.todoDescription ]


view : Model -> Html Msg
view model =
    div []
        [ ul
            []
            (List.map todoView model.todos)
        ]
