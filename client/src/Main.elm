module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Material
import Material.Color as Color
import Material.Layout as Layout
import Material.Scheme as Scheme
import Todo.Update as Todo
import Todos.Api as Todos
import Todos.Model as Todos
import Todos.Update as Todos
import Todos.Update as Todos
import Todos.View as Todos


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { mdl : Material.Model
    , todos : Todos.Model
    }


initialModel : Model
initialModel =
    { mdl = Material.model
    , todos = Todos.initialModel
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.map TodosMsg Todos.get )



-- UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | TodosMsg Todos.Msg
    | TodoMsg Todo.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg' ->
            Material.update msg' model

        TodosMsg msg' ->
            let
                ( updatedModel, cmd ) =
                    Todos.update msg' model.todos
            in
                ( { model | todos = updatedModel }, Cmd.map TodosMsg cmd )

        -- TodoMsg msg' ->
        --     let
        --         ( updatedModel, cmd ) =
        --             Todo.update msg' model.todo
        --     in
        --         ( { model | todo = updatedModel }, Cmd.map TodoMsg cmd )
        _ ->
            ( model, Cmd.none )



-- VIEW


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
                [ Html.map TodoMsg (Todos.view model.todos) ]
            ]
        }
        |> Scheme.topWithScheme Color.Brown Color.Amber


header : Model -> List (Html Msg)
header model =
    [ Layout.row []
        [ Layout.title [] [ text "Haskell Elm Todo App" ]
        ]
    ]
