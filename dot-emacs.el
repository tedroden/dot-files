;; rethinking everything
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) 
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

(setq dotfiles-dir "~/.emacs.d/")
(setq lisp-dir (concat dotfiles-dir "lisp"))
(add-to-list 'load-path lisp-dir)

(setq custom-file (concat dotfiles-dir "custom.el"))
(setq personal-file (concat dotfiles-dir "personal.el"))
(dolist (f (list custom-file personal-file))
  (if (file-exists-p f)
	  (progn (load f)
		 (message (concat "Loaded " f)))
	nil))

;; just for on the mac.
(setq ns-command-modifier 'meta)

;; confirm on exit (i have fat fingers on one of my keyboards)
(setq confirm-kill-emacs 'yes-or-no-p)

;; good mode.
(ido-mode)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
;; a list of packages
(defvar tedroden/packages
  '(auto-complete
    color-theme
    planet-theme
    ac-js2
    ac-ispell
	go-mode
    go-autocomplete
    magit
    markdown-mode
    )
  "Stuff I like")

(if (file-exists-p package-user-dir)
    nil
  (package-refresh-contents))

(dolist (p tedroden/packages)
  (when (not (package-installed-p p))
	(package-install p)))

(load-theme 'planet)

;; auto complete setup. Is this right?
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat lisp-dir "ac-dict"))
(ac-config-default)

;; magit setup. Is this right?
(setq magit-dir (concat dotfiles-dir "external/magit/"))
(add-to-list 'load-path magit-dir)
(require 'magit)
(global-set-key (kbd "C-c m") 'magit-status)


(global-hi-lock-mode 1)	  
(defun tedroden/code-setup ()
  (interactive)
  (highlight-phrase "FIXME" 'hi-yellow)
  (highlight-phrase "Fixme" 'hi-yellow)
  (highlight-phrase "fixme" 'hi-yellow)
  (highlight-phrase "TODO" 'hi-yellow)
  (highlight-phrase "Todo" 'hi-yellow)
  (highlight-phrase "todo" 'hi-yellow))

(add-hook 'prog-mode-hook  'tedroden/code-setup)
(add-hook 'text-mode-hook  'tedroden/code-setup)

;; setup-x p goes to the previous window (opposite of C-x o)
(defun tedroden/prev-window ()
"go to previous window"
(interactive)
(other-window -1))

;; what do you think this does?
(setq-default tab-width 4)

(fset 'yes-or-no-p 'y-or-n-p)

(defun tedroden/edit-dot-emacs ()
  (interactive)
  (find-file (concat dotfiles-dir "init.el")))

;; set up some key bindings (setq default-tab-width 4)
(global-set-key "\M-g" 'goto-line)	
(global-set-key "\M-_" 'shrink-window)
(global-set-key "\M-+" 'enlarge-window)
(global-set-key (kbd "C-x p") 'tedroden/prev-window)
(global-set-key [f4] 'tedroden/edit-dot-emacs)
(global-set-key "\C-ca" 'org-agenda)
