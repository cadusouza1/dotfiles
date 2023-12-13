#!/bin/bash

fzf-previwer() {
    fzf --preview-window=up:60% \
        --preview="bat -n --theme=gruvbox-dark --color=always --paging=never {}"
}

edit() {
    fd -L -H -t f -E .git . $@ | fzf-previwer | xargs -r $EDITOR
}

urls() {
    cat ~/.scripts/urls.txt | rofi -i -dpi 1 -auto-select -normal-window -dmenu | xargs -r $BROWSER

}

urls-new-window() {
    cat ~/.scripts/urls.txt | rofi -i -dpi 1 -auto-select -normal-window -dmenu | xargs -r $BROWSER --new-window
}

cheatsheet() {
    selected=$(compgen -c | gum filter)
    if [[ -n "$selected" ]]; then
        curl "cheat.sh/$selected?T" | nvim -R -c "setfiletype bash" -
    fi
}

manpage() {
    # selected=$(compgen -c | fzf)
    selected=$(man -k . | cut -d' ' -f1 | gum filter)
    if [[ -n "$selected" ]]; then
        man $selected | col -b | nvim -R -c "setfiletype man" -
    fi
}

series() {
    cut -d'>' -f1 ~/.scripts/series.txt |
        rofi -i -dpi 1 -auto-select -normal-window -dmenu |
        xargs -r -I {} grep {} ~/.scripts/series.txt |
        cut -d'>' -f2 |
        sh
}

pdfs() {
    fd -e pdf -L -H -t f -E .git . $@ | gum filter | xargs -I {} -r zathura --fork "{}"
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
