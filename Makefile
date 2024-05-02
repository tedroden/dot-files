PACKAGES=emacs gitignore tmux zsh

all:
	stow --verbose --target=$$HOME --restow ${PACKAGES}

delete:
	stow --verbose --target=$$HOME --delete ${PACKAGES}
