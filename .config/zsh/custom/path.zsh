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

export PATH="$HOME/.local/share/nvim/mason/bin:$PATH"

export PATH="$HOME/vulkansdk/1.3.268.0/x86_64/bin:$PATH"
export PATH="$HOME/google-cloud-sdk/bin:$PATH"

# Paths for my scripts
for p in $(ls -d $HOME/.scripts/**/); do
    export PATH="$p:$PATH"
done
