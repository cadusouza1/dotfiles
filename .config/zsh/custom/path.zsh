# Deno
export DENO_INSTALL="/home/kdu/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"

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
export PATH="$HOME/.cargo/bin:$PATH"

# Odin
export PATH="$HOME/Odin/:$PATH"

# Tmuxifier
export PATH="$HOME/.tmuxifier/bin:$PATH"

# Obsidian
export PATH="$HOME/Obsidian:$PATH"

# Paths for my scripts
for p in $(du $HOME/.scripts/ | cut -f2); do
    export PATH="$p:$PATH"
done
