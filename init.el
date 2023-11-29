;; ~/.emacs.d/init.el --- (this file)

;;; add this to your .bashrc or .zshrc
;; export EDITOR="emacsclient -nw"

;;; I'm currently intalling this emacs:
;; brew tap d12frosted/emacs-plus
;; brew install emacs-plus@30 --with-native-comp
;;
;; osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@30/Emacs.app" at POSIX file "/Applications"
;;
;; Do not reinstall via: `brew reinstall emacs-plus@30 --with-native-comp`
;; Do this: `brew uninstall uninstall emacs-plus@30` and reinstall it.

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

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
  ;; turn it on with...
  (("C-' m" . demap-toggle)))

;; (global-set-key "\M-g" 'goto-line)
(global-set-key "\M-_" 'shrink-window)
(global-set-key "\M-+" 'enlarge-window)
(global-set-key (kbd "C-x p") 'tedroden/prev-window)
(global-set-key [f4] 'ted/edit-dot-emacs)

(global-set-key (kbd "C-c |") 'split-window-right)
(global-set-key (kbd "C-c -") 'split-window-below)
(global-set-key (kbd "C-' |") 'split-window-right)
(global-set-key (kbd "C-' -") 'split-window-below)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-' d") 'insert-date-or-datetime)


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
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; see if we can get away with just the use-package below
;; this is how they say to do it.
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

(use-package ef-themes
  :init
  ;; light dark or nothing
  (ef-themes-load-random 'dark)
;  (setq ef-themes-region '(intense no-extend neutral))
  :bind
    (("C-' t" . ef-themes-load-random))
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
  :bind (("C-c m" . magit-status))
  :custom
  (git-commit-major-mode 'org-mode))

(use-package rainbow-mode
  :hook ((css-mode . rainbow-mode)
		 (sass-mode . rainbow-mode)))

;; setup-x p goes to the previous window (opposite of C-x o)
(defun tedroden/prev-window ()
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
  :bind (("C-c g" . chatgpt-shell))
  :custom
  ((chatgpt-shell-openai-key
	(lambda ()
	  (auth-source-pass-get 'secret "openai-key")))))

(use-package multiple-cursors
  :bind (("C-\-" . 'mc/mark-next-like-this)
		 ("C-0" . 'mc/unmark-next-like-this)))


(setq org-directory (file-truename "~/Dropbox/Org"))
(setq the-list-file (concat org-directory "/the-list.org"))
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
         ("C-c o" . open-the-list)
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
		   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
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


(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))



(defun ok-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

;; This functoin is mostly from: https://gist.github.com/magnars/3292872
(defun goto-line-with-feedback (&optional line)
  "Show line numbers temporarily, while prompting for the line number input. LINE: number"
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
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (typescript-ts-mode . lsp)
		 (python-mode . lsp)
		 (yaml-mode . lsp)
		 (json-mode . lsp)
		 (css-mode . lsp)
		 (bash-mode . lsp)
         ;; if you want which-key integration
;         (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; (use-package sideline)
;; (use-package sideline-lsp)





(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))  ; or lsp-deferred



(use-package flycheck
  :init (global-flycheck-mode))

(use-package company
  :init (global-company-mode)
  :bind (:map company-active-map ("<tab>" . company-complete-selection)))


(with-eval-after-load 'lsp-mode
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace))

(add-hook 'prog-mode-hook #'lsp)

;; Finally
;; Start the server if it's not already started.
(require 'server)
(unless (server-running-p)
  (server-start))

;; (org-roam-dailies-goto-today)

;;; init.el ends here
