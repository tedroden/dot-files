if status is-interactive

    # PATH
    fish_add_path /opt/homebrew/bin /opt/homebrew/sbin
    fish_add_path $HOME/.local/bin
    fish_add_path $HOME/.bun/bin
    fish_add_path $HOME/code/mem

    # Editor — emacsclient with fallback
    set -gx EDITOR "emacsclient -t -a 'emacs -nw'"
    set -gx VISUAL "emacsclient -t -a 'emacs -nw'"

    # Emacs aliases
    alias emacs="emacs -nw"
    alias e="emacsclient -t -a 'emacs -nw'"
    alias ec="emacsclient -t"
    alias emacs-daemon="emacs --daemon"

    # ls
    alias ls="ls --color=auto -F"
    alias l="ls -lah"

    # Docker
    alias dps='docker ps --format "table {{.ID}}\t{{.Image}}\t{{.Status}}\t{{.Names}}"'
    alias dc='docker compose'

    # Git
    alias git-undo-commit='git reset --soft HEAD~1'

    # pyenv
    if type -q pyenv
        pyenv init - | source
    end

    # nvm
    set -gx NVM_DIR "$HOME/.nvm"

    # bun
    set -gx BUN_INSTALL "$HOME/.bun"

end
