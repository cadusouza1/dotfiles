#!/bin/bash

# Nodejs
set VERSION v18.16.0
set DISTRO linux-x64
export PATH="/usr/local/lib/nodejs/node-$VERSION-$DISTRO/bin:$PATH"

# Haskell
export PATH="$HOME/.ghcup/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

# Golang
export PATH="/usr/local/go/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"

# Nim
export PATH="$HOME/.nimble/bin/:$PATH"

# Rust
export PATH="$HOME/.cargo/env:$PATH"

# Odin
export PATH="$HOME/Odin/:$PATH"

# Tmuxifier
export PATH="$HOME/.tmuxifier/bin:$PATH"

# Obsidian
export PATH="$HOME/Obsidian:$PATH"

# Paths for my scripts
for path in (du $HOME/.scripts/ | cut -f2)
    export PATH="$path:$PATH"
end
