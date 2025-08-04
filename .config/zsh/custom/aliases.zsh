alias e="$EDITOR"
alias t="tldr"
alias z="zathura --fork"
alias b="bat --theme=gruvbox-dark --paging=never"
alias v="vim"

# ZSH config
alias zup="source $ZDOTDIR/.zshrc"
alias ls="exa --color=auto -a --group-directories-first"
alias ll="ls -l"
alias sp="sudo pacman"

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
alias saa="sudo apt autoremove"
alias saup="sudo apt update"
alias saug="sudo apt upgrade"

alias ch="curl cheat.sh/${argv[1]}"

function mkcd() {
    mkdir $1 
    cd $1
}
