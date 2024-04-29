;;; init.el --- Emacs configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;
;;
;; Requires Emacs 30+ (due to: `use-package :vc`)
;;
;; add this to your .bashrc or .zshrc

;; export EDITOR="emacsclient -nw"
;;
;;; I'm currently intalling this emacs:
;; brew tap d12frosted/emacs-plus
;; brew install emacs-plus@30 --with-native-comp
;;
;; osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@30/Emacs.app" at POSIX file "/Applications" with properties {name:"Emacs.app"}'
;;

;; DO NOT reinstall, uninstall and install again.
;; Do this: `brew uninstall emacs-plus@30 && rm /Applications/Emacs.app` and reinstall it.

;; Disable the splash screen.
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

(setq warning-minimum-level :emergency)

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

(setq-default c-default-style "k&r")
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default typescript-ts-mode-indent-offset 4)

;; (setq-default c-default-style "k&r")
;; (setq-default tab-width 2)
;; (setq-default indent-tabs-mode nil)
;; (setq-default c-basic-offset 2)
;; (setq-default js-indent-level 2)

(defun ted/edit-dot-emacs ()
  "Quickly edit my dot Emacs file."
  (interactive)
  ;; I use a symlinked file by default, so try to open the OG file
  (let ((dot-emacs (expand-file-name "~/code/dot-files/init.el")))
	;; if not, just open the standard path
	(unless (file-exists-p dot-emacs)
	  (setq dot-emacs (concat dotfiles-dir "init.el")))
	(find-file dot-emacs)))


(defun insert-date-or-datetime (ARG)
  "Insert todays date ARG to get the (American) time."
   (interactive "P")
   (insert (if ARG
               (format-time-string "%Y-%m-%d %H:%M %p")
             (format-time-string "%Y-%m-%d"))))

;; This provides a cute little mini-map (just like a modern editor)
(use-package demap
  :bind
  ;; Turn it on/off with.
  (("C-' m" . demap-toggle)))

; (global-set-key (kbd "C-h") 'delete-backward-char)
; (global-set-key (kbd "C-?") 'help-command)

(global-set-key "\M-_" 'shrink-window)
(global-set-key "\M-+" 'enlarge-window)
; (global-set-key (kbd "C-x p") 'tedroden/prev-window)
(global-set-key [f4] 'ted/edit-dot-emacs)

(global-set-key (kbd "C-c |") 'split-window-right)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c d") 'magit-diff-buffer-file)
(global-set-key (kbd "C-c D") 'insert-date-or-datetime)

(global-set-key (kbd "C-c |") 'split-window-right)
(global-set-key (kbd "C-c -") 'split-window-below)
(global-set-key (kbd "C-' |") 'split-window-right)
(global-set-key (kbd "C-' -") 'split-window-below)



;; Command should be META on the mac
(setq ns-command-modifier 'meta)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;;(add-to-list 'default-frame-alist '(undecorated . t))
;;(add-to-list 'default-frame-alist '(undecorated-round . t))

;; confirm on exit
(setq confirm-kill-emacs 'yes-or-no-p)

;;;; Want line numbers?
;; (global-linum-mode t) ;; Line numbers

;;;; highlight the current line?
;; (global-hl-line-mode t)


;; column number (lives in mode line)
(column-number-mode t)

;; get package stuff ready
(require 'package)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
;; Highest number gets priority (what is not mentioned gets priority 0)
(setq package-archive-priorities
      '(("elpa-devel" . 4)
        ("melpa" . 3)
        ("elpa" . 2)
        ("nongnu" . 1)))
(package-initialize)


;; ;; (require 'use-package)



(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :hook (prog-mode . copilot-mode)
  :bind (("<tab>" . copilot-accept-completion)
		 ("C-TAB" . copilot-accept-completion)))



;; previously, I did `:ensure t` for every `use-package` module
(setq use-package-always-ensure t)

(defun tedroden/no-suspend ()
  "Don't minimize the frame if we hit control-z."
  (interactive)
  (message "Not suspending frame."))

(global-set-key (kbd "C-z") 'tedroden/no-suspend)

;; just for my chromebook!! emacs can do anything.
;; (global-set-key (kbd "<deletechar>") 'backward-kill-word)

(use-package ef-themes
  :init
  ;; light dark or nothing
  (ef-themes-load-random 'dark)
;  (setq ef-themes-region '(intense no-extend neutral))
  :bind
    (("C-' t" . ef-themes-load-random))
  )

;; kind of nice too. but doesn't play well with magit.
;; (use-package ayu-theme
;;   :config (load-theme 'ayu-dark t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-icon (display-graphic-p) "icons if we're not in a terminal")
  ;; set the height

  (doom-modeline-battery t)
  (doom-modeline-height 36)

;; Whether display the `lsp' state. Non-nil to display in the mode-line.
(doom-modeline-lsp t)
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
  :bind (("C-c m" . magit-status))
  :custom
  (git-commit-major-mode 'org-mode))

(use-package rainbow-mode
  :hook ((css-mode . rainbow-mode)
		 (sass-mode . rainbow-mode)))

;; setup-x p goes to the previous window (opposite of C-x o)
(defun tedroden/prev-window ()
  "Go to the previous window."
  (interactive)
  (other-window -1))


(use-package eshell)

;; show eshell right under the current window
(use-package eshell-toggle
  :bind (("C-' e" . eshell-toggle)))

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

(use-package ivy
  :bind
  (("C-o" . 'swiper))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-initial-inputs-alist nil)
  :config
  (ivy-mode nil))

(use-package counsel
  :config
  ;; Ignore some files when doing file searches `C-x C-f`
  ;; Just start typing the file name to show the hidden file(s)
  (setq counsel-find-file-ignore-regexp "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)")
  :bind
  (
   ("C-x b" . 'ivy-switch-buffer)
   ("M-x" . 'counsel-M-x)
   ("C-x C-f" . 'counsel-find-file)
   ("C-x d" . 'counsel-dired)
   ("C-h f" . 'counsel-describe-function)
   ("C-h v" . 'counsel-describe-variable)
   ("M-y" . 'counsel-yank-pop)))

(use-package npm-mode)



;; (use-package 'exec-path-from-shell)

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :custom
  (ibuffer-show-empty-filter-groups nil "Don't show empty groups")
  (ibuffer-saved-filter-groups '(("Home"
								  ("GIT" (name . "^magit-mode"))
                                  
								  ("Dot Files" (filename . "dot-files"))
								  ("Fancy Python" (filename . "/fancy-python"))
								  ("Hands TS" (filename . "/hands-ts"))
								  ("Emacs" (or (filename . "dot-emacs.el")
											   (filename . "init.el")
											   (name . "\*GNU Emacs\*")
											   (name . "\*scratch\*")
											   (name . "\*Messages\*")
											   ))

								  ("Org" (mode . org-mode))
								  ("Eshell" (mode . eshell-mode))
								  ("Man" (name . "\*Man"))
								  ))))

    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "Home")))

;; this is useful if pair programming or demoing
(use-package beacon
  :init
  (beacon-mode t))


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
  :bind (("C-c g" . chatgpt-shell))
  :custom
  ((chatgpt-shell-openai-key
	(lambda ()
	  (auth-source-pass-get 'secret "openai-key")))))

;; I do not like these key bindings. What does VS code do?
(use-package multiple-cursors
  :bind (("C-' 9" . 'mc/mark-next-like-this)
		 ("C-' 0" . 'mc/unmark-next-like-this)))

(setq org-directory (file-truename "~/Dropbox/Org"))
(setq the-list-file (concat org-directory "/the-list.org.gpg"))
(defun open-the-list ()
  "Quickly edit my ~/Org/the-list.org file."
  (interactive)
  (find-file the-list-file))

(require 'org)
(use-package org
  :ensure t
  :demand t
  :bind (("C-c a" . org-agenda)
		 ("C-c c" . org-capture)
         ("C-' o" . open-the-list)
		 :map org-mode-map

		 (("M-F" . org-metaright)
		  ("M-B" . org-metaleft)

		  ("C-c i t" . counsel-org-tag)

		  ;; take these back from co-pillot
		  ("<tab>" . org-cycle)
		  ("S-<tab>" . org-shifttab)
		  ("C-<tab>" . org-global-cycle)

		  ("M-P" . org-metaup)
		  ("M-N" . org-metadown)

		  ("C-c o" . org-table-insert-row)

		  ("C-c t i" . org-table-insert-row)
		  ("C-c t p" . org-table-move-row-up)
		  ("C-c t n" . org-table-move-row-down)
		  ("C-c X" . org-latex-export-to-pdf)
		  ))

  :init
  (unbind-key "C-'" org-mode-map)
  (unbind-key "C-," org-mode-map)
  (setq org-ellipsis " ▾")
  (setq org-latex-pdf-process '("pdflatex -output-directory=pdfs %f"))
  (setq org-time-stamp-formats '("%Y-%m-%d %a" . "%Y-%m-%d %a %I:%M%p"))
  (setq org-archive-location "archive/%s_archive::")
  (setq org-agenda-files (list org-directory))
  (setq org-agenda-remove-tags nil)
  ;; hide stars and indent on startup:
  (setq org-startup-indented t)
  (setq org-hide-leading-stars t)



  :config
  (setq org-capture-templates
	    '(("t" "TODO" entry (file+headline tasks-file "Tasks")
		   "* TODO %?\n  %i\n  %a")
		  ("f" "Fancy Hands" entry (file+headline tasks-file "Fancy Hands")
		   "* TODO %?\n  %i\n  %a")
		  ("g" "Grow" entry (file+headline tasks-file "Grow")
		   "* TODO %?\n  %i\n  %a")
		  ("s" "Shopping" entry (file+headline tasks-file "Tasks")
		   "* TODO %?%(org-set-tags \"BUY\")\n")
		  ))
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
							  "#+title: Daily Notes %<%Y-%m-%d>\n#+created: %U\n\n"))))

  (setq org-roam-capture-templates
		'(("d" "default" plain "* %?"
		   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg"
							  "#+created: %U\n\n#+title: ${title}\n\n")
		   :unnarrowed t))))


;;;;
;; https://takeonrules.com/2022/01/11/resolving-an-unable-to-resolve-link-error-for-org-mode-in-emacs/
(defun tedroden/force-org-rebuild-cache ()
  "Rebuild the `org-mode' and `org-roam' cache."
  (interactive)
  (org-id-update-id-locations)
  (org-roam-db-clear-all)
  (org-roam-db-sync)
  (org-roam-update-org-id-locations))

;; (use-package activity-watch-mode
;;   :ensure t
;;   :config
;;   (global-activity-watch-mode t))

(use-package dotenv-mode)

(use-package dired-narrow
  :bind (:map dired-mode-map
			  ("F" . dired-narrow))
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package typescript-ts-mode
  :ensure t)

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/code/fh")))

;; (use-package ag)

(use-package counsel-projectile
  :config
  (counsel-projectile-mode)

  :bind
  (("C-c k" . 'counsel-projectile-rg)
   ("M-p" . 'counsel-projectile-find-file) ;; i think this is close to vs code, right?
   ("C-c C-f" . 'counsel-projectile-find-file)))

(use-package org-roam-ui
  :bind
  (("C-c n g" . org-roam-ui-open))
  :config
  (setq org-roam-ui-sync-theme t
		org-roam-ui-follow t
		org-roam-ui-update-on-save t
		org-roam-ui-open-on-start t))


;; (use-package pdf-tools
;;   :ensure t
;;   :config
;;   (pdf-tools-install))



(defun ok-goto-line (line)
  "Go to the specified LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

;; This function is mostly from: https://gist.github.com/magnars/3292872
(defun goto-line-with-feedback (&optional line)
  "Show line numbers temporarily, while prompting for the LINE number input."
  (interactive "P")
  (if line
	  (ok-goto-line line)
	(unwind-protect
		(progn
		  (nlinum-mode 1)
		  (ok-goto-line (read-number "Goto line: ")))
	  (nlinum-mode -1))))

(use-package nlinum
  :bind
  (("M-g" . goto-line-with-feedback)))

(use-package treemacs)
(use-package treemacs-projectile)
(use-package dockerfile-mode)

(use-package emojify
  :bind
  (("C-c E" . emojify-insert-emoji)))




;; (use-package eglot
;;   :ensure t)

;; (setq major-mode-remap-alist
;;       '((python-mode . python-ts-mode)))

;; built in
(require 'treesit)
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; (setq major-mode-remap-alist
;;  '((yaml-mode . yaml-ts-mode)
;;    (bash-mode . bash-ts-mode)
;;    (js2-mode . js-ts-mode)
;;    (typescript-mode . typescript-ts-mode)
;;    (json-mode . json-ts-mode)
;;    (css-mode . css-ts-mode)
;;    (python-mode . python-ts-mode)))

                                        ;(use-package eat)

;; built in stuff...
(defun open-current-file-with-sudo-tramp ()
  "Open the currently visited file with sudo:: method in TRAMP,
   but refuse to open files in the home directory.
   (A copilot/chatgpt colab... )"
  (interactive)
  (when buffer-file-name
	(let ((file-path (buffer-file-name)))
	  (unless (string-prefix-p (expand-file-name "~") file-path)
		(find-alternate-file ;; this will kill the current buffer, use switch-to-buffer if you don't want that.
		 (concat "/sudo::" file-path)))
	  (when (string-prefix-p (expand-file-name "~") file-path)
		(message "Don't bring sudo into your home directory")))))

(global-set-key (kbd "C-' s") 'open-current-file-with-sudo-tramp)
(setq tramp-auto-save-directory (expand-file-name "~/.emacs.d/tramp-autosave"))

;; (use-package which-key                 ;
;;   :config
;;   (which-key-mode)
;;   (which-key-setup-minibuffer)
;;   )


;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-restart 'ignore)
  (setq lsp-modeline-code-actions-enable  nil)
  (setq lsp-apply-edits-after-file-operations nil)
  
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (typescript-ts-mode . lsp)
		 (python-mode . lsp)
		 (yaml-mode . lsp)
		 (json-mode . lsp)
		 (css-mode . lsp)
		 (bash-mode . lsp)
		 (sh-mode . lsp)
         )
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))  ; or lsp-deferred
(with-eval-after-load 'lsp-mode
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace))


(add-hook 'prog-mode-hook #'lsp)


(use-package treesit-auto
  :config
  (global-treesit-auto-mode 1))

;; (use-package eglot)


(use-package flycheck
  :init (global-flycheck-mode))


(use-package vundo
  :ensure t
  :bind
  (("C-' v" . vundo)
   ("C-' r" . redo)))

(use-package company
  :init (global-company-mode)
  :bind (:map company-active-map ("<enter>" . company-complete-selection)))


;; ;; With use-package:
(use-package company-box
   :hook (company-mode . company-box-mode))


(use-package kbd-mode
  :vc (:url "https://github.com/kmonad/kbd-mode" :rev :newest))

(use-package password-store
  :ensure t
  :bind (("C-' p" . password-store-copy)))


(use-package wrap-region
  :ensure t
  :config
  (wrap-region-global-mode t))

(use-package pinentry
  :ensure t
  :config
  (pinentry-start))

(use-package buffer-move
  :bind
  (("C-c <up>" . buf-move-up)
   ("C-c <down>" . buf-move-down)
   ("C-c <left>" . buf-move-left)
   ("C-c <right>" . buf-move-right)))

(setenv "GPG_AGENT_INFO" nil)


;;;; start chat gpt
(defun my-xref-customizations ()
  ;; Disable copilot-mode first, ensure this matches how you disable it.
  ;; Check if copilot-mode is available before trying to disable.
  (when (featurep 'copilot)
    (copilot-mode -1))

  ;; Customize TAB behavior in Xref buffers
  ;; Replace `'desired-tab-function` with the actual function you want for TAB.
  ;; For example, `xref-next-line` could be a useful default for navigating references.
  (local-set-key (kbd "RET") 'xref-quit-and-goto-xref)
  (local-set-key (kbd "TAB") 'xref-goto-xref)
  ;; Optionally, set SHIFT-TAB to go to the previous line, mirroring TABs navigation.
  (local-set-key (kbd "<backtab>") 'xref-previous-line))

;; Add the custom function to xref buffer mode hook.
(add-hook 'xref--xref-buffer-mode-hook 'my-xref-customizations)

;;; end chapt gpt


;; Finally
;; Start the server if it's not already started.
(require 'server)
(unless (server-running-p)
  (server-start))

;; (org-roam-dailies-goto-today)

;;; init.el ends here
