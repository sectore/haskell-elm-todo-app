module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Todo.NewTodo as NewTodo
import Todos.Todos as Todos


type alias Model =
    { todos : Todos.Model
    , newTodo : NewTodo.Model
    }


initialModel : Model
initialModel =
    { todos = Todos.initialModel
    , newTodo = NewTodo.initialModel
    }


type Msg
    = TodosMsg Todos.Msg
    | NewTodoMsg NewTodo.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TodosMsg msg' ->
            let
                ( updatedModel, cmd ) =
                    Todos.update msg' model.todos
            in
                ( { model | todos = updatedModel }, Cmd.map TodosMsg cmd )

        NewTodoMsg msg' ->
            let
                ( newTodoModel, newTodoCmd ) =
                    NewTodo.update msg' model.newTodo
            in
                case msg' of
                    -- add new todo to todo list w/o an extra request
                    NewTodo.SaveTodoDone id' ->
                        let
                            newTodo =
                                model.newTodo.todo

                            newTodo' =
                                { newTodo | id = id' }

                            ( todosModel, todosCmd ) =
                                Todos.update (Todos.AddNewTodo newTodo') model.todos
                        in
                            ( { model
                                | newTodo = newTodoModel
                                , todos = todosModel
                              }
                            , Cmd.batch
                                [ Cmd.map TodosMsg todosCmd
                                , Cmd.map NewTodoMsg newTodoCmd
                                ]
                            )

                    _ ->
                        ( { model | newTodo = newTodoModel }, Cmd.map NewTodoMsg newTodoCmd )


view : Model -> Html Msg
view model =
    div []
        [ Html.map NewTodoMsg (NewTodo.view model.newTodo)
        , Html.map TodosMsg (Todos.listView model.todos)
        ]


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.map TodosMsg Todos.getTodos )


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
