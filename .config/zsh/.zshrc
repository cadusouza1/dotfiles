autoload -U colors && colors

PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "

# Basic auto/tab complete:
# _comp_options+=(globdots)		# Include hidden files.
autoload -U compinit
setopt share_history
setopt autocd
setopt globdots
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit

HISTSIZE=10000000
SAVEHIST=10000000
HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/history"
setopt inc_append_history

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

autoload -z edit-command-line
zle -N edit-command-line
# bindkey "\ee" edit-command-line

# Custom scripts
for file in $(ls $ZDOTDIR/custom/**.zsh); do
    . $file
done

. /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
. $ZDOTDIR/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

export FZF_ALT_C_COMMAND=""
[ -f ~/.fzf.zsh ] && . ~/.fzf.zsh

bindkey -v
