function fuzzy-edit
    fd -H -L -t f . $argv | fzf --height=10 | xargs -r $EDITOR
    commandline -f repaint
end

function fuzzy-cd
    cd (fd -H -L -t d . $argv | fzf --height=10)
    commandline -f repaint
end

set bindings \
    "jw:$EDITOR    ~/.xmonad/xmonad.hs" \
    "jt:$EDITOR    ~/.config/tmux/tmux.conf" \
    "jn:fuzzy-edit ~/.config/nvim/" \
    "js:fuzzy-edit ~/school/" \
    "jf:fuzzy-edit ~/.config/fish/" \
    "jc:fuzzy-edit ~/.config/ ~/.scripts/ ~/.xmonad/ ~/.local/bin/" \
    "jp:fuzzy-edit ~/.local/share/nvim/site/pack/packer/start/" \
    "ks:fuzzy-cd   ~/school/" \
    "kc:fuzzy-cd   ~/.config/ ~/.scripts/ ~/.xmonad/ ~/.local/" \
    "kp:fuzzy-cd   ~/.local/share/nvim/site/pack/packer/start/" \
    "u:history-search-backward" \
    "d:history-search-forward" \
    "l:accept-autosuggestion" \
    "w:forward-word" \
    "W:forward-bigword"

for binding in $bindings
    set key (echo $binding | cut --delimiter=":" -f1)
    set fn (echo $binding | cut --delimiter=":" -f2)

    bind -M insert \e$key $fn
end

