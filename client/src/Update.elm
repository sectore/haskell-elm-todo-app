module Update exposing (update, Msg(..))

import Material
import Model exposing (Model)
import Todos.Update as Todos
import Todo.Update as Todo


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
