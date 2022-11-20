#!/bin/bash

# Haskell
export PATH="$HOME/.ghcup/bin:$PATH"

export PATH="$HOME/.local/bin:$PATH"

# Golang
export PATH="/usr/local/go/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"

# Nim
export PATH="$HOME/.nimble/bin/:$PATH"

# Paths for my scripts
for path in (du $HOME/.scripts/ | cut -f2)
    export PATH="$path:$PATH"
end
