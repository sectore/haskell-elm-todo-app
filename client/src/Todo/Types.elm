module Todo.Types exposing (..)

import Http


type alias Todo =
    { id : Int
    , completed : Bool
    , description : String
    }


type Msg
    = Update String
    | Save
    | SaveDone Int
    | SaveFailed Http.Error
    | DeleteDone Http.Response
    | DeleteFailed Http.Error
    | UpdateDone Http.Response
    | UpdateFailed Http.Error
    | Cancel
    | NoOp
