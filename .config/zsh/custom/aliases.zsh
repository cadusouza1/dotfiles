alias e="$EDITOR"
alias t="tldr"
alias r="rich"
alias z="zathura --fork"
alias b="bat"
alias v="vim"

# ZSH config
alias zup="source $ZDOTDIR/.zshrc"
alias ls="exa --color=auto -a --group-directories-first"

lt() {
    if test -n "$argv[1]"; then
        dept=$argv[1]
    else
        dept=2
    fi

    exa --color=auto --icons -a --group-directories-first -T -R -L $dept -l --no-permissions --no-user --no-time --git
}

# Cargo
alias cgr="cargo run"
alias cgb="cargo build"
alias cgbr="cargo build --release"
alias cgi="cargo install"
alias cgu="cargo uninstall"
alias cga="cargo add"
alias cgc="cargo clippy"
alias cgrm="cargo rm"
alias cgin="cargo init"

alias sai="sudo apt install"
alias sar="sudo apt remove" 
alias sap="sudo apt purge" 
alias saup="sudo apt update"
alias saug="sudo apt upgrade"

alias tmf="tmuxifier"

alias ch="curl cheat.sh/${argv[1]}"
alias mkcd="mkdir $1 && cd $1"
