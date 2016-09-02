module Todo.Update exposing (Msg, update)

import Todo.Model as Todo


type Msg
    = NoOp


update : Msg -> Todo.Model -> ( Todo.Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
