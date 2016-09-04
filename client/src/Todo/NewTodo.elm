module Todo.NewTodo exposing (..)

import Html exposing (..)
import Material
import Material.Color as Color
import Material.Grid as Grid
import Material.Options as Options
import Material.Textfield as Textfield


type alias Model =
    { mdl : Material.Model
    , description : Maybe String
    }


initialModel : Model
initialModel =
    { mdl = Material.model
    , description = Nothing
    }


type Msg
    = Mdl (Material.Msg Msg)
    | Input String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg' ->
            Material.update msg' model

        Input value ->
            ( { model | description = Just value }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Textfield.render Mdl
        [ 1 ]
        model.mdl
        [ Textfield.label "Enter new Todo"
        , Textfield.onInput Input
        , Options.cs "mdl-cell mdl-cell--12-col"
        , Options.inner
            [ Options.id "hello-input"
            , Color.text Color.black
            ]
        ]
