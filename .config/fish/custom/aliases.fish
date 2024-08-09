alias e="$EDITOR"
alias t="tldr"
alias r="rich"
alias z="zathura --fork $argv[1]"

# Fish config
alias fup="source $FISHDOTDIR/config.fish"

alias ls="exa --color=auto -a --group-directories-first"

function lt 
    if test -n "$argv[1]"
        set dept $argv[1]
    else
        set dept 2
    end

    exa --color=auto --icons -a --group-directories-first -T -R -L $dept -l --no-permissions --no-user --no-time --git
end

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

function ch 
    curl cheat.sh/$argv[1]
end

function mkcd
    mkdir $argv[1]
    cd $argv[1]
end

function m
    man $argv[1] | col -b | nvim -R -c "setfiletype man" -
end
