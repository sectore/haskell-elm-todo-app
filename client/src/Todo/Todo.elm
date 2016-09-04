module Todo.Todo exposing (..)

import Material


type alias ViewModel =
    { mdl : Material.Model
    , todo : Model
    }


type alias Model =
    { id : Int
    , todoCompleted : Bool
    , todoDescription : String
    }
