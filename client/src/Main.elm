module Main exposing (..)

import Api
import Html exposing (..)
import Html.App as Html
import Material
import Material.Color as Color
import Material.Layout as Layout
import Material.Options as Options
import Material.Scheme as Scheme
import Material.Grid as Grid
import Material.Typography as Typo
import Maybe
import Todo.NewTodo as NewTodo
import Todos.Todos as Todos


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
        { header = headerView model
        , drawer = []
        , tabs = ( [], [] )
        , main = mainView model
        }
        |> Scheme.topWithScheme Color.Brown Color.Amber


headerView : Model -> List (Html Msg)
headerView model =
    [ Layout.row []
        [ Options.styled p
            [ Typo.display1, Typo.uppercase ]
            [ text "Haskell Elm Todo App" ]
        ]
    ]


mainView : Model -> List (Html Msg)
mainView model =
    [ Grid.grid []
        [ Grid.cell [ Grid.offset Grid.All 1, Grid.size Grid.All 10 ]
            [ Html.map NewTodoMsg (NewTodo.view model.newTodo)
            , h1 [] [ text <| Maybe.withDefault "" model.newTodo.description ]
            , Html.map TodosMsg (Todos.listView model.todos)
            ]
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
