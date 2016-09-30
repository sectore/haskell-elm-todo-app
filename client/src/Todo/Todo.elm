module Todo.Todo exposing (..)

import Html.Attributes exposing (..)
import Html exposing (..)


type alias ViewModel =
    { todo : Model
    }


type alias Model =
    { id : Int
    , completed : Bool
    , description : String
    }


type Msg
    = NoOp


view : ViewModel -> Html Msg
view model =
    input
        [ value model.todo.description
        ]
        []
