module Todo.Todo exposing (..)

import Http


type alias ViewModel =
    { todo : Model
    }


type alias Model =
    { id : Int
    , completed : Bool
    , description : String
    }
