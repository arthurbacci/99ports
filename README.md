# 99ports

[uberports](https://github.com/takusuman/uberports) in Haskell.

# Why haskell?

- Compiled
- Faster than shell

# Compiling

## With stack

Install dependencies:

    stack install directory
    stack install wreq
    stack install network-uri

Compile:

    stack exec ghc -- main.hs -o 99ports

Install:

    sudo cp 99ports /usr/bin/
