# _still in progress ..._

# haskell-elm-todo-app

Todo app build with [Haskell](https://www.haskell.org/) ([Servant](http://haskell-servant.github.io/), [Persistent](https://hackage.haskell.org/package/persistent) on server-side and [Elm](http://elm-lang.org/) on client-side.

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

- run tests

``` shell
stack build --test
```

### DB queries

by using [httpie](https://github.com/jkbrzt/httpie):

``` shell

# add a todo
http POST localhost:3000/todo/ completed:=false description="my todo"

# get a todo
http localhost:3000/todo/1

# delete a todo
http DELETE localhost:3000/todo/1

# update a todo
http PUT localhost:3000/todo/1 description="any other description" completed:=true

# get all todos
http localhost:3000/todos

```


### Build and run client-side app

- Goto project folder

``` shell
cd {project}/client/
```

- Build project:

``` shell
npm i
```

- Run app

``` shell
npm start

```

Open [http://localhost:3333](http://localhost:3333).


## Acknowledge

### Helpful Haskell / Servant stuff

- Haskell + Persistent: [http://www.yesodweb.com/book/persistent](http://www.yesodweb.com/book/persistent)

- School of Haskell "[Querying an existing database](https://www.schoolofhaskell.com/school/advanced-haskell/persistent-in-detail/existing-database)"

- Example Servant + Persistent: [https://github.com/haskell-servant/example-servant-persistent/](https://github.com/haskell-servant/example-servant-persistent/)

- Example Servant + Persistent by [Matt Parsons](https://github.com/parsonsmatt/): [https://github.com/parsonsmatt/servant-persistent](https://github.com/parsonsmatt/servant-persistent)

- Example Servant + Elm: [https://github.com/haskell-servant/example-servant-elm](https://github.com/haskell-servant/example-servant-elm)

- "Todobackend" with Servant: [https://github.com/jhedev/todobackend-haskell/tree/master/todobackend-servant](https://github.com/jhedev/todobackend-haskell/tree/master/todobackend-servant)

- Album app (Haskell + Elm) by [Magnus Rundberget](https://github.com/rundis): [https://github.com/rundis/albums](https://github.com/rundis/albums)

- DB example of "[5 Ways to Test Application Code that Accesses a Database in Haskell](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/)"


### Helpful Elm stuff

- Blog post "[How I Structure Elm Apps](http://blog.jenkster.com/2016/04/how-i-structure-elm-apps.html)" by [Kris Jenkins](https://github.com/krisajenkins)

- [Advanced example](https://github.com/krisajenkins/elm-dialog/tree/master/examples/Advanced) of `elm-dialog` by [Kris Jenkins](https://github.com/krisajenkins)
