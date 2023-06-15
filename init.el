
;; ~/.emacs.d/init.el --- (this file)

;;;;  currently intalling this emacs:
;; brew tap d12frosted/emacs-plus
;; brew install emacs-plus@29 --with-native-comp
;; 

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; Start the server if it's not already started.
(require 'server)
(unless (server-running-p)
    (server-start))
   
;; Remember: you can press [F4] to open this file from emacs.
;; (info "(eintr) Top")   ; lisp tutorial

;;; Code:
;; turn off a lot of the UI
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))


(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)


(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))


;;; add this to your zshrc
;; # If I call emacs proper, I probably want emacs proper
;; alias emacs="emacs -nw"

;; # this doesn't work because of the alias? so it's a function
;; # alias e="emacsclient -c -n ${1:-.}"
;; unalias e

;; function e() {
;; 	emacsclient -c -n "${1:-.}"
;; }


;; setup custom/personal/etc.
(setq-default dotfiles-dir "~/.emacs.d/"
	      custom-file (concat dotfiles-dir "custom.el")
	      personal-file (concat dotfiles-dir "personal.el"))

(dolist (f (list custom-file personal-file))
  (if (file-exists-p f)
	  (progn (load f)
		 (message (concat "Loaded " f)))
    nil))

;; all "yes" or "no" questions should be y/n
(fset 'yes-or-no-p 'y-or-n-p)

(global-hi-lock-mode 1)

(setq-default c-default-style "k&r"
			  c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)

(defun ted/edit-dot-emacs ()
  "Quickly edit my dot Emacs file."
  (interactive)
  ;; I use a symlinked file by default, so try to open the OG file
  (let ((dot-emacs (expand-file-name "~/code/dot-files/init.el")))
	;; if not, just open the standard path
	(unless (file-exists-p dot-emacs)
	  (setq dot-emacs (concat dotfiles-dir "init.el")))
	(find-file dot-emacs)))


(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-_" 'shrink-window)
(global-set-key "\M-+" 'enlarge-window)
(global-set-key (kbd "C-x p") 'tedroden/prev-window)
(global-set-key [f4] 'ted/edit-dot-emacs)
(global-set-key (kbd "C-c |") 'split-window-right) ;; should be removed in favor of exwm?
(global-set-key (kbd "C-c -") 'split-window-below);; should be removed in favor of exwm?
(global-set-key (kbd "C-h m") 'man) ;; normally this is describe mode.


;; Command should be META on the mac
(setq ns-command-modifier 'meta)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;;;; not great on a mac
;;(add-to-list 'default-frame-alist '(undecorated . t))
;;(add-to-list 'default-frame-alist '(undecorated-round . t))


;; confirm on exit
(setq confirm-kill-emacs 'yes-or-no-p)

;;;; Want line numbers?
;; (global-linum-mode t) ;; Line numbersthis is good for teaching, but i don't generally want it.

;;;; highlight the current line?
;; (global-hl-line-mode nil)

;; column number (lives in mode line)
(column-number-mode t)


;; get package stuff ready
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))



;; ;; ;; thanks federico
(unless (package-installed-p 'use-package)
  (progn
	(package-refresh-contents)
	(package-install 'use-package)))


(require 'use-package)


(use-package quelpa-use-package
  :ensure t)

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  
  :hook (prog-mode . copilot-mode)
  
  :bind (("<tab>" . copilot-accept-completion)
	 ("C-TAB" . copilot-accept-completion-by-word)))

;; previously, I did `:ensure t` for every `use-package` module
(setq use-package-always-ensure t)


(defun tedroden/no-suspend ()
  "Don't minimize the frame if we hit control-z."
  (interactive)
  (message "Not suspending frame. You're welcome"))

(global-set-key (kbd "C-z") 'tedroden/no-suspend)

;; just for my chromebook!! emacs can do anything.
(global-set-key (kbd "<deletechar>") 'backward-kill-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme related stuff

(use-package doom-themes
  :ensure t
  :init
  ;; (load-theme 'doom-snazzy)
  ;; (load-theme 'doom-dark+)
  ;;  (load-theme 'doom-material)
  ;; (load-theme 'doom-1337)
  ;; (load-theme 'doom-badger)
  ;;  (load-theme 'doom-dracula)
  ;;  (load-theme 'doom-rouge)
  ;;  (load-theme 'doom-molokai)
;;  (load-theme 'doom-ephemeral)
  ;; (load-theme 'doom-outrun-electric)
;;   (load-theme 'doom-peacock)
 ;; (load-theme 'doom-molokai)
 ;; (load-theme 'doom-nord)
 ;; (load-theme 'doom-spacegrey)
 ;; (load-theme 'doom-tomorrow-day)
  (load-theme 'doom-tomorrow-night)
 ;; (load-theme 'doom-vibrant)
 )

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-icon t "icons")
  (doom-modeline-height 32 "height")
  (doom-modeline-buffer-encoding nil "don't show 'UTF-8' everywhere")
  (doom-modeline-icon (display-graphic-p))
  )

;; fixme: get rid of this if we don't have a battery

;; (use-package battery
;;   :ensure t
;;   :config
;;   (display-battery-mode))

(use-package time
  :ensure t
  :custom
  (display-time-default-load-average nil "Don't show load average")

  :config
  (display-time-mode))

(use-package yaml-mode
  :mode (("\\.yaml$'" . yaml-mode)
	 ("\\.yml$'" . yaml-mode)))

;;; end theme related
;;;;;;;;;;;;;;;;;;;;;;


;; great for quickly switching windows if you've got more than 2
;; (use-package ace-window
;;   :ensure t
;;   :bind ("M-o" . ace-window))
(use-package switch-window
  :bind (("M-o" . switch-window))
  :custom
  (switch-window-shortcut-style 'qwerty "use letters instead of numbers"))

;; super cool search if you can see where you want to go.
(use-package avy
  :bind ("C-/" .'avy-goto-char-2))

;; The package is "python" but the mode is "python-mode":
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; (use-package artbollocks-mode
;;   :defer 4 ;; we don't actually need this, so don't load it for a while
;;   :custom
;;   (artbollocks-jargon nil)
;;   :config

;;   (progn
;;     (setq artbollocks-weasel-words-regex
;;           (concat "\\b" (regexp-opt
;;                          '("one of the"
;;                            "should"
;;                            "just"
;;                            "sort of"
;;                            "a lot"
;;                            "probably"
;;                            "maybe"
;;                            "perhaps"
;;                            "I think"
;;                            "really"
;;                            "pretty"
;;                            "nice"
;;                            "action"
;;                            "utilize"
;;                            "leverage") t) "\\b"))
;; 	(add-hook 'text-mode-hook 'artbollocks-mode)))


;;;; show icons in dired! (requires all-theicons-dired)
;; (use-package all-the-icons-dired
;; :hook ((dired-mode . all-the-icons-dired-mode)))

(use-package all-the-icons-ibuffer
  :ensure t
  :init (all-the-icons-ibuffer-mode 1))


;;;;
;;;;;; magit setup. Is this right?
(use-package magit
  :bind (("C-c m" . magit-status)))

(use-package rainbow-mode
  :hook ((css-mode . rainbow-mode)
		 (sass-mode . rainbow-mode)))



(defun tedroden/code-setup ()
  "Highlight certain phrases."
  (interactive)
  (highlight-phrase "FIXME" 'black)
  (highlight-phrase "Fixme" 'black)
  (highlight-phrase "fixme" 'black)
  )

;;;;;; (defun tedroden/writer-mode ()
;;;;;;   (interactive)
;;;;;;   (markdown-mode)
;;;;;;   (visual-line-mode)
;;;;;;   (visual-fill-column-mode))
;;;;
;;;;(defun tedroden/writer-mode ()
;;;;  (interactive)
;;;;  (message "not loading writer-mode"))
;;;;

(add-hook 'prog-mode-hook  'tedroden/code-setup)
(add-hook 'text-mode-hook  'tedroden/code-setup)


;; setup-x p goes to the previous window (opposite of C-x o)
(defun tedroden/prev-window ()
  "go to previous window"
  (interactive)
  (other-window -1))

;; not sure when this is actually used.
(setq-default tab-width 4)


(use-package eshell
  :bind  (("C-!" . eshell))
  :config
  (progn
	;; (add-hook 'eshell-mode-hook 'eshell-mode-hook-func)
	(add-hook 'eshell-preoutput-filter-functions
			  'ansi-color-apply)
	;; requires `eshell-git-prompt`
	))


;; show eshell right under the current window
(use-package eshell-toggle
  :bind (("C-c e" . eshell-toggle)))

;; (use-package ido
;;   :bind
;;   ( ("C-x C-f" . ido-find-file) ))

;; (use-package helm
;;   :bind (("C-x C-f" . helm-find-files)
;;   ("C-x b" . helm-mini)
;;   ("M-x" . helm-M-x)
;;   ("C-h a" . helm-apropos)
;;   ("M-y". helm-show-kill-ring) ;; eh...
;;   ("M-i" . helm-swoop-without-pre-input)
;;   ("C-s" . isearch-forward)
	 
;; 	 :map helm-map ("<tab>" . helm-execute-persistent-action))
;;   :config 
;; ;  (setq helm-ff-file-name-history-use-recentf t)
;; ;  (setq helm-buffers-fuzzy-matching t)
;;   (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
;;   (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;;   (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
;;   (helm-mode t))

;; failing (install the icon files... "m-x icons install" something should get you close
;; (use-package all-the-icons-ivy-rich
;;   :init (all-the-icons-ivy-rich-mode 1))

 (set-frame-font "Monaco 14")


(use-package ivy-rich)
(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package nerd-icons-ivy-rich
  :ensure t
  :init
  (nerd-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1))


(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))




(use-package ivy
  :bind
  (("C-o" . 'swiper))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-initial-inputs-alist nil)
  :config
  (ivy-mode nil))

(use-package counsel
  :bind
  (("C-c k" . 'counsel-projectile-ag)
   ("C-x b" . 'counsel-switch-buffer)
   ("M-x" . 'counsel-M-x)
   ("C-x C-f" . 'counsel-find-file)
   ("C-c C-f" . 'counsel-projectile-find-file)   
   ("C-x d" . 'counsel-dired)      
   ("C-h f" . 'counsel-describe-function)
   ("C-h v" . 'counsel-describe-variable)
   ("M-y" . 'counsel-yank-pop)))


(use-package projectile
  :config
  (projectile-mode t)
  :bind (("C-c p" . projectile-command-map)))

(use-package npm-mode)

; built in
(require 'treesit)

(use-package flycheck
  :init (global-flycheck-mode))


; (use-package 'exec-path-from-shell)

;;;;
;;;;
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :custom
  (ibuffer-never-show-predicates '("*helm") "don't show helm")
  (ibuffer-show-empty-filter-groups nil "Don't show empty groups")
  (ibuffer-saved-filter-groups '(("Buffers"
								  ("FANCY" (filename . "code/fh/fancy"))
								  ("HANDS" (filename . "code/fh/hands"))
								  ("Fancy Hands Code" (filename . "code/fh/fancyhands"))
								  ("Dot Files" (filename . "dot-files"))								  
								  ("Emacs" (or (filename . "dot-emacs.el")
											   (filename . "init.el")
											   (name . "\*GNU Emacs\*")
											   (name . "\*scratch\*")
											   (name . "\*Messages\*")
											   ))
								  ("exwm" (mode . exwm-mode))
								  ("GIT" (mode . magit-mode))
								  ("Mail" (name . "\*notmuch"))	 
								  ("Org" (mode . org-mode))
								  ("Eshell" (mode . eshell-mode))
								  ("Man" (name . "\*Man"))	 
								  ))))


;;;;;; this is useful if pair programming or demoing
;; (use-package beacon
;;   :init
;;   (beacon-mode t))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :interpreter ("node" . js2-mode))

;; put the cursor where it was last time you visited a file
(use-package saveplace
  :init (save-place-mode 1)
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-limit nil)))

(use-package rjsx-mode
  :defer t)

;; "control-c left arrow" brings you bakc to your
;; last window configuration
(use-package winner
  :init (winner-mode))


(use-package dired
  :ensure nil ;; don't make `use-package` go find this, it's part of emacs
  :config
  ;;;; show hidden files in dired?

  (put 'dired-find-alternate-file 'disabled nil))


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-hide-markup-in-view-modes t)
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(use-package dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(use-package google-this
  :bind (("C-x g" . google-this)))

;; FIXME: You can just use `customize-variable` to set the key for now
;;        I would love to get auth-source-pass working though
(use-package chatgpt-shell
    :ensure t)
    ;; :custom
    ;; ((chatgpt-shell-openai-key
    ;;   (lambda ()
    ;;     (auth-source-pass-get 'secret "openai-key")))))


(use-package multiple-cursors
  :bind (("C-\-" . 'mc/mark-next-like-this)
		 ("C-0" . 'mc/unmark-next-like-this)))




(use-package org
  :ensure t
  :demand t
  :init
  (setq org-directory "~/Dropbox/Org")
  (setq org-agenda-files (list org-directory))
  ;; default notes file
  (setq org-default-notes-file (concat org-directory "/Notes.org"))
  ;; default todo file
  (setq org-default-tasks-file (concat org-directory "/Tasks.org"))

  :config
  (defun my-org-template (type headline)
    (let ((template "* %?\n  %i\n  %a"))
      (when (string= type "t")
        (setq template (concat "* TODO %?\n  %i\n  %a")))
      `(,type ,headline entry (file+headline ,(concat org-directory "/" headline ".org") ,headline)
         ,template)))

  (setq org-capture-templates
        (list (my-org-template "t" "Tasks")
              (my-org-template "n" "Notes")
              (my-org-template "i" "Ideas")))
  
  (setq org-startup-folded 'showall)
  (require 'org-agenda))

(global-set-key (kbd "C-c a") 'org-agenda)  ;; Bind C-c a to org-agenda
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c t") 'org-todo)
(global-set-key (kbd "M-p") 'org-backward-heading-same-level)
(global-set-key (kbd "M-n") 'org-forward-heading-same-level)

(use-package auth-source-1password
  :ensure t
  :config
  (auth-source-pass-enable))

(use-package activity-watch-mode
  :ensure t
  :config
  (global-activity-watch-mode t))

(use-package dotenv-mode)

(use-package dired-narrow
  :bind (("C-c C-n" . dired-narrow)))



(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; ;;; init.el ends here
