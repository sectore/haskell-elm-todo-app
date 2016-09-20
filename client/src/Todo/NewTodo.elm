module Todo.NewTodo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type alias Model =
    { description : Maybe String
    }


initialModel : Model
initialModel =
    { description = Nothing
    }


type Msg
    = Input String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input value ->
            ( { model | description = Just value }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    input [ placeholder "Enter new Todo", onInput Input ] []
