module Todo.Todo exposing (..)


type alias ViewModel =
    { todo : Model
    }


type alias Model =
    { id : Int
    , completed : Bool
    , description : String
    }
