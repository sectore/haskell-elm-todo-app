module Model exposing (..)

import Material
import Todos.Model as Todos


type alias Model =
    { mdl : Material.Model
    , todos : Todos.Model
    }


initialModel : Model
initialModel =
    { mdl = Material.model
    , todos = Todos.initialModel
    }
