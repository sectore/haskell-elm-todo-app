# _still in progress ..._

# haskell-elm-todo-app

Todo app build with [Servant](http://haskell-servant.github.io/) ([Haskell](https://www.haskell.org/)) on server-side and [Elm](http://elm-lang.org/) on client-side

## Instructions

### Build and run server-side app


``` shell
stack build
stack exec haskell-elm-todo-app
```

### DB queries

``` shell
# insert an user
curl -H 'Content-type: application/json' localhost:3000/user/add --data '{"name": "Alice", "age": 42}'

# get an user
curl -H 'Content-type: application/json' localhost:3000/user/get/Alice

# get all users
curl -H 'Content-type: application/json' localhost:3000/users/

```
