#!/bin/sh

export PLATFORM=`uname` 

function log {
    echo "-> " $1
}

function age {
    log "Backing up $1"
    rm -Rf $1.old
    mv -v $1 $1.old
    
}

function emacs {
    log "Installing .emacs"
    if [ -d ~/.emacs.d ]; then
		age ~/.emacs.d
    fi
    mkdir ~/.emacs.d
    ln -sv `pwd`/dot-emacs.el ~/.emacs.d/init.el
    ln -sv `pwd`/helm-init.el ~/.emacs.d/helm-init.el    
}

function xmodmap {
    log "Installing xmodmap for chromebook pixel"
    if [ -f ~/.xmodmap ]; then
		age ~/.xmodmap
    fi
    ln -sv `pwd`/dot-xmodmap  ~/.xmodmap
}

function xinitrc {
    log "Installing xinitrc"
    if [ -f ~/.xinitrc ]; then
		age ~/.xinitrc
    fi
    ln -sv `pwd`/dot-xinitrc  ~/.xinitrc
}
function zshrc {
    log "Installing .zshrc"
    if [ -f ~/.zshrc ]; then
		age ~/.zshrc
    fi
    ln -sv `pwd`/dot-zshrc  ~/.zshrc
}

# this happens on mac or linux
zshrc
emacs

# this stuff only makes sense on linux
if [ "$PLATFORM" = "Linux" ]; then 
    xinitrc
    xmodmap
fi

