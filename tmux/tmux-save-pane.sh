#!/bin/zsh
SAVEDIR=~/Dropbox/mem/$(date +%Y/%m/%d)
mkdir -p "$SAVEDIR"

SESSION=$(tmux display-message -p '#{session_name}')
WINDOW=$(tmux display-message -p '#{window_index}')
PANE=$(tmux display-message -p '#{pane_index}')
TIMESTAMP=$(date +%Y%m%d-%H%M%S)

FILENAME="_tmux_-${TIMESTAMP}-${SESSION}-w${WINDOW}-p${PANE}.txt"

tmux capture-pane -S -32768
tmux save-buffer "${SAVEDIR}/${FILENAME}"

echo "Saved: ${SAVEDIR}/${FILENAME}"
