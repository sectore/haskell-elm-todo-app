module Todo.Model exposing (Model)


type alias Model =
    { id : Int
    , todoCompleted : Bool
    , todoDescription : String
    }
