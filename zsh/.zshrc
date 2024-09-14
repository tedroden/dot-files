# -*- mode: sh -*-

# This uses oh-my-zsh, mostly for the built in completions and themes.
#
# Goals:
#
# 1. disable the aliases.
# 2. use the gaudiest themes possible.
# 3. Setup nvm and penv (and gcloud sdk)
#

# Path to your oh-my-zsh installation.
ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="kphoen"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
ZSH_THEME_RANDOM_CANDIDATES=()

# Case-sensitive completion.
CASE_SENSITIVE="true"

# update automatically without asking
zstyle ':omz:update' mode auto

# I'll set my own aliases. I don't need 1000 undocumented ones.
zstyle ':omz:*' aliases no

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
COMPLETION_WAITING_DOTS="true"

plugins=(gnu-utils docker colored-man-pages gh)

source $ZSH/oh-my-zsh.sh

unsetopt share_history

# User configuration
# Set the homebrew paths.
export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:$PATH"

# emacs
export EDITOR="emacsclient -nw"
alias e="emacsclient -n"   # open in existing frame, no waiting
alias et="emacsclient -t"  # open in terminal
alias ew="emacsclient"     # open regular, but wait for close

# ls
alias ls="ls --color=auto -F"
alias l="ls -lah"

# nvm
export NVM_DIR="${HOME}/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # works with zsh

# pyenv
eval "$(pyenv init -)"

# gcloud
CLOUDSDK_HOME="${HOME}/code/deps/google-cloud-sdk"

# The next line updates PATH for the Google Cloud SDK.
if [ -f "${CLOUDSDK_HOME}/path.zsh.inc" ]; then . "${CLOUDSDK_HOME}/path.zsh.inc"; fi

# The next line enables shell command completion for gcloud.
if [ -f "${CLOUDSDK_HOME}/completion.zsh.inc" ]; then . "${CLOUDSDK_HOME}/completion.zsh.inc"; fi
