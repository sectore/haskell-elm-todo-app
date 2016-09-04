module View exposing (view)

import Html exposing (..)
import Html.App as Html
import Material.Color as Color
import Material.Layout as Layout
import Material.Layout as Layout
import Material.Scheme as Scheme
import Model exposing (Model, initialModel)
import Todos.View as Todos
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        ]
        { header = header model
        , drawer = []
        , tabs = ( [], [] )
        , main =
            [ div []
                [ Html.map TodoMsg (Todos.view model.todos) ]
            ]
        }
        |> Scheme.topWithScheme Color.Brown Color.Amber


header : Model -> List (Html Msg)
header model =
    [ Layout.row []
        [ Layout.title [] [ text "Haskell Elm Todo App" ]
        ]
    ]
