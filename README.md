# _still in progress ..._

# haskell-elm-todo-app

Todo app build with [Servant](http://haskell-servant.github.io/) ([Haskell](https://www.haskell.org/)) on server-sdie and [Elm](http://elm-lang.org/) on client-side

## Instructions

### Build and run server-side


``` shell
stack build
stack exec haskell-elm-todo-app
```

### DB data

``` shell
# insert an user
curl -H 'Content-type: application/json' localhost:3000/user/add --data '{"name": "Alice", "age": 42}'

# get a user
curl -H 'Content-type: application/json' localhost:3000/user/get/Alice

# get all users
curl -H 'Content-type: application/json' localhost:3000/users/

```
