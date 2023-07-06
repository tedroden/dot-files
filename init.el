
;; ~/.emacs.d/init.el --- (this file)

;;; add this to your .bashrc or .zshrc
;; export EDITOR="emacsclient -nw"

;;; I'm currently intalling this emacs:
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


;; setup custom/personal/etc.
(setq-default dotfiles-dir (file-truename "~/.emacs.d/")
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

;; Command should be META on the mac
(setq ns-command-modifier 'meta)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;;(add-to-list 'default-frame-alist '(undecorated . t))
;;(add-to-list 'default-frame-alist '(undecorated-round . t))

;; confirm on exit
(setq confirm-kill-emacs 'yes-or-no-p)

;;;; Want line numbers?
;; (global-linum-mode t) ;; Line numbersthis is good for teaching, but i don't generally want it.

;;;; highlight the current line?
										; (global-hl-line-mode t)

;; column number (lives in mode line)
(column-number-mode t)

;; get package stuff ready
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; ;; see if we can get away with just the use-package below
;; ;; this is how they say to do it. 
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))


;; (require 'use-package)

(use-package quelpa)
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
;; (global-set-key (kbd "<deletechar>") 'backward-kill-word)

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
;;  (load-theme 'doom-spacegrey)
  (load-theme 'doom-badger)
;; (load-theme 'doom-tomorrow-day)
  ;;  (load-theme 'doom-tomorrow-night)
  ;; (load-theme 'doom-vibrant)
  )

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-icon (display-graphic-p) "icons if we're not in a terminal")
  ;; set the height
  (doom-modeline-height 32 "height")
  (doom-modeline-battery t)
  (doom-modeline-buffer-encoding nil "don't show 'UTF-8' everywhere"))

;; FIXME: get rid of this if we don't have a battery
;; (use-package battery
;;   :ensure t
;;   :config
;;   (display-battery-mode nil))

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

(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode))

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

;; Is this how I should do this? I don't know.
(use-package python
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode))

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


;; failing (install the icon files... "m-x icons install" something should get you close
;; (use-package all-the-icons-ivy-rich
;;   :init (all-the-icons-ivy-rich-mode 1))

(set-frame-font "Monaco 14")


(use-package ivy-rich)
(use-package nerd-icons)

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

;; (use-package ivy
;;   :bind
;;   (("C-o" . 'swiper))
;;   :custom
;;   (ivy-use-virtual-buffers t)
;;   (ivy-initial-inputs-alist nil)
;;   :config
;;   (ivy-mode nil))

;; (use-package counsel
;;   :bind
;;   (("C-x b" . 'counsel-switch-buffer)
;;    ("M-x" . 'counsel-M-x)
;;    ("C-x C-f" . 'counsel-find-file)
;;    ("C-x d" . 'counsel-dired)
;;    ("C-h f" . 'counsel-describe-function)
;;    ("C-h v" . 'counsel-describe-variable)
;;    ("M-y" . 'counsel-yank-pop)))

(use-package npm-mode)

;; built in
(require 'treesit)

(use-package flycheck
  :init (global-flycheck-mode))

;; (use-package 'exec-path-from-shell)

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :custom
  (ibuffer-show-empty-filter-groups nil "Don't show empty groups")
  (ibuffer-saved-filter-groups '(("Buffers"
								  ("Dot Files" (filename . "dot-files"))
								  ("Emacs" (or (filename . "dot-emacs.el")
											   (filename . "init.el")
											   (name . "\*GNU Emacs\*")
											   (name . "\*scratch\*")
											   (name . "\*Messages\*")
											   ))
								  ("exwm" (mode . exwm-mode))
								  ("GIT" (mode . magit-mode))
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

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-hide-markup-in-view-modes t)
  :bind (:map markdown-mode-map
			  ("C-c C-e" . markdown-do)))

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package google-this
  :bind (("C-x g" . google-this)))

(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pass-get 'secret "open-ai")))))


(use-package multiple-cursors
  :bind (("C-\-" . 'mc/mark-next-like-this)
		 ("C-0" . 'mc/unmark-next-like-this)))

(use-package org
  :ensure t
  :demand t
  :bind
  (("C-c a" . org-agenda)  ;; Bind C-c a to org-agenda
   ("C-c c" . org-capture)

  ;; this will override copilot if you want it to.
  (:map org-mode-map
        ("<tab>" . org-cycle)
        ("S-<tab>" . org-shifttab)
        ("C-<tab>" . org-global-cycle)))



  :init
  ;; setup org-indent-mode
  (setq org-startup-indented t)
  ;; American time formats
  (setq org-time-stamp-formats '("%Y-%m-%d %a" . "%Y-%m-%d %a %I:%M%p"))
  (setq org-directory (file-truename "~/Dropbox/Org"))
  (setq org-archive-location "archive/%s_archive::")
  (setq org-agenda-files (list org-directory))
  ;; default notes file
  (setq org-default-notes-file (concat org-directory "/Notes.org"))
  ;; default todo file
  (setq org-default-tasks-file (concat org-directory "/Tasks.org"))

  :config
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-default-tasks-file "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("n" "Note" entry (file+headline org-default-notes-file "Notes")
           "* %?\n  %i\n  %a")))
;;   (setq org-capture-default-template "t")
;;  (setq org-startup-folded 'showall)
  (require 'org-agenda))



(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename"~/Dropbox/mem"))
  (org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  (org-startup-folded 'content)

  :bind (("C-c n l" . org-roam-buffer-toggle)
		 ("C-c n f" . org-roam-node-find)
		 ("C-c n i" . org-roam-node-insert)
		 ("C-c n c" . org-roam-capture)
		 ("C-c n j" . org-roam-dailies-capture-today)
		 :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow)
		 )
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)

  :config
    (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-setup)
  (org-roam-db-autosync-mode)

;;  (setq org-roam-graph-executable "/usr/local/bin/dot")
  (setq org-id-locations-file  (concat dotfiles-dir ".org-id-locations"))
  (setq org-roam-node-display-template
		(concat "${title:*} "
				(propertize "${tags:10}" 'face 'org-tag)))

  (setq org-roam-dailies-capture-templates
		'(("d" "default" entry
		   "* %?"
		   :if-new (file+head "%<%Y-%m-%d>.org"
							  "#+filetags: ${filetags}\n#+title: %<%Y-%m-%d>\n"))))
  (setq org-roam-capture-templates
		'(("d" "default" plain "* %?"
		   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
							  "#+created: %U\n#+filetags: ${filetags} \n#+title: ${title}\n\n")
		   :unnarrowed t))))

(use-package activity-watch-mode
  :ensure t
  :config
  (global-activity-watch-mode t))

(use-package dotenv-mode)

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("F" . dired-narrow))
  :config
  (put 'dired-find-alternate-file 'disabled nil))

;; (use-package editorconfig
;;   :ensure t
;;   :config
;;   (editorconfig-mode 1))

(use-package typescript-ts-mode
  :ensure t)

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/code/fh")))

(use-package ag)

(use-package counsel-projectile
  :config
  (counsel-projectile-mode)

  :bind
  (("C-c k" . 'counsel-projectile-ag)
   ("C-c C-f" . 'counsel-projectile-find-file)))

(use-package org-roam-ui
  :bind
  (("C-c n g" . org-roam-ui-open))
  :config
  (setq org-roam-ui-sync-theme t
		org-roam-ui-follow t
		org-roam-ui-update-on-save t
		org-roam-ui-open-on-start t))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(server-start)

;; ;;; init.el ends here
