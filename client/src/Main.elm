module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Http
import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Decode.Pipeline as Pipeline
import Task
import Material
import Material.Layout as Layout
import Material.Scheme as Scheme
import Material.Color as Color
import Material.List as Lists


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
    , mdl : Material.Model
    }


type alias Todo =
    { id : Int
    , todoCompleted : Bool
    , todoDescription : String
    }


initialModel : Model
initialModel =
    { mdl = Material.model
    , todos = []
    , selectedTodo = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, getTodos )



-- UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | FetchTodosDone (List Todo)
    | FetchTodosFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg' ->
            Material.update msg' model

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
    Lists.li []
        [ text todo.todoDescription ]


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        ]
        { header = header model
        , drawer = []
        , tabs = ( [], [] )
        , main =
            [ div []
                [ Lists.ul
                    []
                    (List.map todoView model.todos)
                ]
            ]
        }
        -- mdl color scheme
        |> Scheme.topWithScheme Color.Brown Color.Amber


header : Model -> List (Html Msg)
header model =
    [ Layout.row []
        [ Layout.title [] [ text "Haskell Elm Todo App" ]
        ]
    ]
