module Todos.Model exposing (Model, initialModel)

import Todo.Model as Todo


type alias Model =
    { todos : List Todo.Model
    , selectedTodo : Maybe Todo.Model
    }


initialModel : Model
initialModel =
    { todos = []
    , selectedTodo = Nothing
    }
