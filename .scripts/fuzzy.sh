#!/bin/bash

fzf-previwer() {
    fzf --preview-window=up:60% \
        --preview="bat -n --theme=gruvbox-dark --color=always --paging=never {}"
}

edit() {
    fd -L -H -t f -E .git . $@ | fzf-previwer | xargs -r $EDITOR
}

urls() {
    rofi -dpi 1 -auto-select -normal-window -dmenu -input ~/.scripts/urls.txt | xargs -r $BROWSER
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

series() {
    cut -d'>' -f1 ~/.scripts/series.txt |
        rofi -dpi 1 -auto-select -normal-window -dmenu |
        xargs -r -I {} grep {} ~/.scripts/series.txt |
        cut -d'>' -f2 |
        xargs -r $BROWSER
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
