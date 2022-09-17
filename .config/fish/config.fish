# Don't Randomly Turn off
xset s off
xset -dpms
xset s noblank

# No key repeat
xset r off

# Environment variables set everywhere
export EDITOR="nvim"
export TERMINAL="alacritty"
export BROWSER="firefox"

# XDG Paths
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share

# remap caps to escape
setxkbmap -option caps:escape
setxkbmap -option ctrl:ralt_rctrl

export FISHDOTDIR=$HOME/.config/fish

# Haskell
export PATH="$HOME/.ghcup/bin:$PATH"

export PATH="$HOME/.local/bin:$PATH"

# Golang
export PATH="$PATH:/usr/local/go/bin"
export PATH="$HOME/go/bin:$PATH"

source $FISHDOTDIR/themes/robbyrussell/functions/fish_prompt.fish
set -U fish_greeting

setxkbmap -model abnt2 -layout br -variant abnt2

fish_vi_key_bindings

source $FISHDOTDIR/aliases.fish

#set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin /home/work/.ghcup/bin $PATH # ghcup-env

if test -n "$NVIM_LISTEN_ADDRESS"
  set -x MANPAGER "/usr/local/bin/nvr -c 'Man!' -o -"
end

export FZF_DEFAULT_COMMAND="fdfind -H . $HOME"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="fdfind -H -t d . $HOME"


set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin /home/work/.ghcup/bin $PATH # ghcup-env
