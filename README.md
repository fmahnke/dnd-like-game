# dnd-like-game
I don't know Haskell. I want to know Haskell. I'm trying to make a game like Dungeons and Dragons to learn Haskell.

Build and run the server:

```shell
ghc --make src/Server.hs -o dist/server && dist/server 65000
```

Build and run the client:

```shell
ghc --make src/Client.hs -o dist/client && dist/client 127.0.0.1 65000 username
```

