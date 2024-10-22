# Ted Roden's dotfiles

It's mostly my [init.el](emacs/dot-emacs.d/init.el) file.

 - [`📝 emacs`](emacs/dot-emacs.d/init.el) my `init.el`
 - [`🚫 gitignore`](gitignore/.gitignore) a global `.gitignore`
 - [`🖥️ tmux`](tmux/.tmux.conf) a nice `.tmux.conf`
 - [`🐚 zsh`](zsh/.zshrc) a very simple `.zshrc` (using `oh-my-zsh`)

## Usage

 1. Copy or link the files wherever you want.
 2. or use `stow` (`brew install stow`):
 
Use [`install.sh`](install.sh) to install and remove them:

 - `sh ./install.sh emacs` 👉 creates emacs link
 - `sh ./install.sh emacs-delete` 👉 removes it

