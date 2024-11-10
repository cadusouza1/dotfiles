# XDG Paths
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_STATE_HOME=$HOME/.state
export DEFAULT_TMUX_SESSION_NAME="0"

export FISHDOTDIR=$HOME/.config/fish

source $FISHDOTDIR/themes/robbyrussell/functions/fish_prompt.fish
set -U fish_greeting

fish_vi_key_bindings

for file in (du -a $FISHDOTDIR/custom/ | grep "\.fish" | cut -f2)
    source $file
end

export FZF_DEFAULT_COMMAND="fdfind -H . $HOME"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="fdfind -H -t d . $HOME"

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin /home/kdu/.ghcup/bin $PATH # ghcup-env

# Check if I'm on my termux
[ (whoami) = "u0_a374" ] || [ -z "$TMUX" ] && tmux new-session -A -s $DEFAULT_TMUX_SESSION_NAME
