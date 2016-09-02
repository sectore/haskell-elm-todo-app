module Todo.Api exposing (decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Todo.Model as Todo


decoder : Decoder Todo.Model
decoder =
    Pipeline.decode Todo.Model
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "todoCompleted" Decode.bool
        |> Pipeline.required "todoDescription" Decode.string
