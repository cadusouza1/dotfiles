#!/bin/bash

fzf-previwer() {
    fzf --preview-window=up:60% \
        --preview="bat -n --theme=gruvbox-dark --color=always --paging=never {}"
}

edit() {
    fd -L -H -t f -E .git . $1 | fzf-previwer | xargs -r nvim
}

urls() {
    rofi -dpi 1 -auto-select -normal-window -dmenu -input ~/urls.txt | xargs -r firefox
}

cheatsheet() {
    selected=$(compgen -c | fzf)
    if [[ -n "$selected" ]]; then
        curl "cheat.sh/$selected?T" | nvim -R -c "setfiletype bash" -
    fi
}

manpage() {
    selected=$(compgen -c | fzf)
    if [[ -n "$selected" ]]; then
        man $selected | col -b | nvim -R -c "setfiletype man" -
    fi
}

# Check if the function exists (bash specific)
if declare -f "$1" >/dev/null; then
    # call arguments verbatim
    "$@"
else
    # Show a helpful error
    echo "'$1' is not a known function name" >&2
    exit 1
fi