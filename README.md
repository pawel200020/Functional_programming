# Functional Programming
Exercises from laboratories at Jagiellonian Uniwesity. (GHCI version - 9.0.1, Erl version - v12.2)
Includes:
<ol>
<li>Simple haskell functions which can be interpreted by interpreter
<li>Short apps with main fuction, and monad IO usage
<li>Simple erlang apps
<li>Simple erlang apps which runs some processes which send and receive data from each other.
</ol>

## Usage haskell interpreter
```
ghci program.hs
```
where program is a file name which you want to load.

## Usage haskell apps with Main:
```
ghc Main.hs 
./Main
```
where Main is a file name which you want to compile.

## Usage erlang apps:
```
erl
c(ex1).
ex1:start(args).
```
where ex1 is a file name which you want to compile.