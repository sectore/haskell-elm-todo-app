module Todo.State exposing (..)

import Monocle.Lens exposing (Lens)
import Todo.Types exposing (..)
import Return exposing (Return)


-- Model


emptyTodo : Todo
emptyTodo =
    Todo -1 False ""


initialNewTodo : Todo
initialNewTodo =
    emptyTodo



-- Lens


idL : Lens Todo Int
idL =
    Lens .id (\x m -> { m | id = x })


completedL : Lens Todo Bool
completedL =
    Lens .completed (\x m -> { m | completed = x })


descriptionL : Lens Todo String
descriptionL =
    Lens .description (\x m -> { m | description = x })



-- update


update : Msg -> Todo -> Return Msg Todo
update msg todo =
    Return.singleton todo
        |> case msg of
            Update value ->
                Return.map <| .set descriptionL value

            Cancel ->
                Return.map <| always emptyTodo

            _ ->
                Return.zero
