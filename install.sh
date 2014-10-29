#!/bin/sh

function log {
	echo "-> " $1
}

function emacs {
	log "Installing .emacs"
	if [ ! -d "~/.emacs.d" ]; then
		mkdir ~/.emacs.d
	fi
	ln -sv `pwd`/dot-emacs.el ~/.emacs.d/init.el
}

function xmodmap {
	log "Installing xmodmap for chromebook pixel"
	ln -sv `pwd`/dot-xmodmap  ~/.xmodmap
}

function xinitrc {
	log "Installing xinitrc"
	ln -sv `pwd`/dot-xinitrc  ~/.xinitrc
}
function zshrc {
	log "Installing .zshrc"
	ln -sv `pwd`/dot-zshrc  ~/.zshrc
}

zshrc
xinitrc
xmodmap
emacs
