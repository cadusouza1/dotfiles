alias e="$EDITOR"
alias t="tldr"
alias r="rich"

# Fish config
alias fconf="e $FISHDOTDIR/config.fish"
alias fup="source $FISHDOTDIR/config.fish"

alias ls="exa --color=auto -a --group-directories-first"
alias ll="exa --color=auto --icons -a --group-directories-first"

function lt 
    if test -n "$argv[1]"
        set dept_level $argv[1]
    else
        set dept_level 2
    end

    exa --color=auto --icons -a --group-directories-first -T -R -L $dept_level -l --no-permissions --no-user --no-time --git
end

## Cargo
alias cgr="cargo run"
alias cgb="cargo build"
alias cgbr="cargo build --release"
alias cgi="cargo install"
alias cgu="cargo uninstall"
alias cga="cargo add"
alias cgrm="cargo rm"
alias cgc="cargo clippy"

alias nplug="cd $XDG_DATA_HOME/nvim/site/pack/packer/start"
alias nconf="cd $XDG_CONFIG_HOME/nvim/lua && e ../init.lua && cd -"

alias pp="source ~/.scripts/pp.fish"

alias sain="sudo apt install"
alias sarm="sudo apt remove" 
alias saup="sudo apt update"
alias saug="sudo apt upgrade"
alias sapu="sudo apt purge"

function mkcd
    mkdir $argv[1] 
    cd $argv[1]
end

function ch 
    curl cheat.sh/$argv[1]
end
