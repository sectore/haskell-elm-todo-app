module Main exposing (..)

import Api
import Maybe
import Html exposing (..)
import Html.App as Html
import Material
import Material.Layout as Layout
import Material.Color as Color
import Material.Scheme as Scheme
import Todo.NewTodo as NewTodo
import Todos.Todos as Todos


-- import Todo.Update as Todo


type alias Model =
    { mdl : Material.Model
    , todos : Todos.Model
    , newTodo : NewTodo.Model
    }


initialModel : Model
initialModel =
    { mdl = Material.model
    , todos = Todos.initialModel
    , newTodo = NewTodo.initialModel
    }


type Msg
    = Mdl (Material.Msg Msg)
    | TodosMsg Todos.Msg
    | NewTodoMsg NewTodo.Msg



-- | TodoMsg Todo.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TodosMsg msg' ->
            let
                ( updatedModel, cmd ) =
                    Todos.update msg' model.todos
            in
                ( { model | todos = updatedModel }, Cmd.map TodosMsg cmd )

        NewTodoMsg msg' ->
            let
                ( updatedModel, cmd ) =
                    NewTodo.update msg' model.newTodo
            in
                ( { model | newTodo = updatedModel }, Cmd.map NewTodoMsg cmd )

        _ ->
            ( model, Cmd.none )


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
                [ h1 [] [ text <| Maybe.withDefault "" model.newTodo.description ]
                , Html.map TodosMsg (Todos.listView model.todos)
                ]
            ]
        }
        |> Scheme.topWithScheme Color.Brown Color.Amber


header : Model -> List (Html Msg)
header model =
    [ Layout.row []
        [ Layout.title [] [ text "Haskell Elm Todo App" ]
        , Layout.spacer
        , Html.map NewTodoMsg (NewTodo.view model.newTodo)
        ]
    ]


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.map TodosMsg Api.getTodos )


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
