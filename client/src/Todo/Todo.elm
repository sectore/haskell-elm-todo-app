module Todo.Todo exposing (..)


type alias Model =
    { id : Int
    , completed : Bool
    , description : String
    }


emptyTodo : Model
emptyTodo =
    Model -1 False ""
