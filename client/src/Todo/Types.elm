module Todo.Types exposing (..)

import Http


type alias Todo =
    { id : Int
    , completed : Bool
    , description : String
    }


type alias NewTodo =
    { todo : Todo
    , request : Bool
    }


type Msg
    = Input String
    | Enter
    | Cancel
    | SaveDone Int
    | SaveFail Http.Error
    | NoOp
