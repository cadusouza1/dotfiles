fuzzy-edit() {
    fd -H -L -t f . $argv | gum filter | xargs -r $EDITOR
    # commandline -f repaint
}

fuzzy-cd() {
    cd (fd -H -L -t d . $argv | fzf --height=10)
    # commandline -f repaint
}

bindings=(
    "jw:$EDITOR ~/.xmonad/xmonad.hs" 
    "jt:$EDITOR ~/.config/tmux/tmux.conf" 
    "jn:fuzzy-edit ~/.config/nvim/" 
    "js:fuzzy-edit ~/.scripts/" 
    "jf:fuzzy-edit ~/.config/fish/" 
    "jc:fuzzy-edit ~/.config/ ~/.scripts/ ~/.xmonad/ ~/.local/bin/" 
    "jp:fuzzy-edit ~/.local/share/nvim/site/pack/packer/start/" 
    "ks:fuzzy-cd ~/.scripts/" 
    "kc:fuzzy-cd ~/.config/ ~/.scripts/ ~/.xmonad/ ~/.local/" 
    "kp:fuzzy-cd ~/.local/share/nvim/site/pack/packer/start/" 
    "u:history-search-backward" 
    "d:history-search-forward" 
    "l:accept-autosuggestion" 
    "a:la && commandline -f repaint" 
    "w:forward-word" 
    "W:forward-bigword" 
    "e:edit-command-line")

for binding in $bindings; do
    key=$(cut --delimiter=":" -f1 <<< $binding)
    fn=$(cut --delimiter=":" -f2 <<< $binding)

    bindkey "\e$key" $fn
done

