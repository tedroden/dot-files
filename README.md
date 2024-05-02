# Ted Roden's dotfiles

It's mostly my [init.el](emacs/.emacs.d/init.el) file.

 - [`📝 emacs`](emacs/.emacs.d/init.el) my `init.el`
 - [`🚫 gitignore`](gitignore/.gitignore) a global `.gitignore`
 - [`🖥️ tmux`](tmux/.tmux.conf) a nice `.tmux.conf`
 - [`🐚 zsh`](zsh/.zshrc) a very simple `.zshrc` (using `oh-my-zsh`)


## Usage

 1. Copy or link the files wherever you want.
 2. Using `stow` (`brew install stow`):
 
There is a [`Makefile`](Makefile) to install and remove them:

 - `make all` 👉 creates links
 - `make delete` 👉 removes them

