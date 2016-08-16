# _still in progress ..._

# haskell-elm-todo-app

Todo app build with [Servant](http://haskell-servant.github.io/) ([Haskell](https://www.haskell.org/)) on server-side and [Elm](http://elm-lang.org/) on client-side

## Instructions

### Build and run server-side app

- Goto project folder

``` shell
cd {project}/server/
```

- Build project:

``` shell
stack build
```

- Execute app

``` shell
stack exec haskell-elm-todo-app
```

- OR use live reloading w/ [halive](https://github.com/lukexi/halive)

``` shell
halive src/Main.hs
```

### DB queries

by using [httpie](https://github.com/jkbrzt/httpie):

``` shell
# insert an user
http POST localhost:3000/user/add name=Alice age:=42

# get an user
http localhost:3000/user/get/Alice

# get all users
http localhost:3000/users/

# delete an user
http DELETE localhost:3000/user/Alice

# add a todo
http POST localhost:3000/todo/ completed:=false description="my todo 1"

```

## Acknowledge

### Haskell / Servant stuff

- Haskell + Persistent: [http://www.yesodweb.com/book/persistent](http://www.yesodweb.com/book/persistent)

- School of Haskell "[Querying an existing database](https://www.schoolofhaskell.com/school/advanced-haskell/persistent-in-detail/existing-database)"

- Example Servant + Persistent: [https://github.com/haskell-servant/example-servant-persistent/](https://github.com/haskell-servant/example-servant-persistent/)

- Example Servant + Persistent by Matt Parsons: [https://github.com/parsonsmatt/servant-persistent](https://github.com/parsonsmatt/servant-persistent)

- Example Servant + Elm: [https://github.com/haskell-servant/example-servant-elm](https://github.com/haskell-servant/example-servant-elm)

- "Todobackend" with Servant: [https://github.com/jhedev/todobackend-haskell/tree/master/todobackend-servant](https://github.com/jhedev/todobackend-haskell/tree/master/todobackend-servant)

- Album Haskell Elm app: [https://github.com/rundis/albums](https://github.com/rundis/albums)
