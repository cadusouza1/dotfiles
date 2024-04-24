# Don't Randomly Turn off
xset s off
xset -dpms
xset s noblank

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

source $FISHDOTDIR/themes/robbyrussell/functions/fish_prompt.fish
set -U fish_greeting

setxkbmap -model abnt2 -layout br -variant abnt2

fish_vi_key_bindings

for file in (du -a $FISHDOTDIR/custom/ | grep "\.fish" | cut -f2)
    source $file
end

export FZF_DEFAULT_COMMAND="fdfind -H . $HOME"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="fdfind -H -t d . $HOME"

eval (tmuxifier init - fish)

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin /home/kdu/.ghcup/bin $PATH # ghcup-env

# opam configuration
source /home/kdu/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
