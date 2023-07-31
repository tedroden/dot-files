
;; ~/.emacs.d/init.el --- (this file)

;;; add this to your .bashrc or .zshrc
;; export EDITOR="emacsclient -nw"

;;; I'm currently intalling this emacs:
;; brew tap d12frosted/emacs-plus
;; brew install emacs-plus@29 --with-native-comp
;;
;; osascript -e "tell application \"Finder\" to make alias file to (POSIX file \"/opt/homebrew/Cellar/emacs-plus@29/29.0.90/Emacs.app\") at POSIX file \"/Applications\""

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
;; (global-hl-line-mode t)

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

(use-package ef-themes
  :init
  ;; light dark or nothing
  (ef-themes-load-random 'dark)
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

;; setup-x p goes to the previous window (opposite of C-x o)
(defun tedroden/prev-window ()
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
  ("C-x b" . 'counsel-switch-buffer)
  ("M-x" . 'counsel-M-x)
  ("C-x C-f" . 'counsel-find-file)
   ("C-x d" . 'counsel-dired)
   ("C-h f" . 'counsel-describe-function)
   ("C-h v" . 'counsel-describe-variable)
   ("M-y" . 'counsel-yank-pop)))

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
      (auth-source-pass-get 'secret "openai-key")))))

(use-package multiple-cursors
  :bind (("C-\-" . 'mc/mark-next-like-this)
		 ("C-0" . 'mc/unmark-next-like-this)))

(use-package org
  :ensure t
  :demand t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)

         :map org-mode-map

         (("M-F" . org-metaright)
          ("M-B" . org-metaleft)

		  ;; take these back from co-pillot
          ("<tab>" . org-cycle)
          ("S-<tab>" . org-shifttab)
          ("C-<tab>" . org-global-cycle)
		  ("C-c t i" . org-table-insert-row)
		  ("C-c t p" . org-table-move-row-up)
		  ("C-c t n" . org-table-move-row-down)
		  ))

  :init
  (setq org-startup-indented t)
  (setq org-time-stamp-formats '("%Y-%m-%d %a" . "%Y-%m-%d %a %I:%M%p"))
  (setq org-directory (file-truename "~/Dropbox/Org"))
  (setq org-archive-location "archive/%s_archive::")
  (setq org-agenda-files (list org-directory))
  (setq org-default-notes-file (concat org-directory "/Notes.org"))
  (setq org-default-tasks-file (concat org-directory "/Tasks.org"))
  :config
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-default-tasks-file "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("n" "Note" entry (file+headline org-default-notes-file "Notes")
           "* %?\n  %i\n  %a")))
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

;; ;;;;;;;;;;;;;;;;;;;;
;; ;; Enable vertico (config from vertico/README.org)
;; ;; START OF VERTICO CONFIG
;; (use-package vertico
;;   :init
;;   (vertico-mode)

;;   ;; Different scroll margin
;;   ;; (setq vertico-scroll-margin 0)

;;   ;; Show more candidates
;;   ;; (setq vertico-count 20)

;;   ;; Grow and shrink the Vertico minibuffer
;;   ;; (setq vertico-resize t)

;;   ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
;;   ;; (setq vertico-cycle t)
;;   )

;; ;; Persist history over Emacs restarts. Vertico sorts by history position.
;; (use-package savehist
;;   :init
;;   (savehist-mode))

;; ;; A few more useful configurations...
;; (use-package emacs
;;   :init
;;   ;; Add prompt indicator to `completing-read-multiple'.
;;   ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
;;   (defun crm-indicator (args)
;;     (cons (format "[CRM%s] %s"
;;                   (replace-regexp-in-string
;;                    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
;;                    crm-separator)
;;                   (car args))
;;           (cdr args)))
;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (setq minibuffer-prompt-properties
;;         '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;   ;; Enable recursive minibuffers
;;   (setq enable-recursive-minibuffers t))

;; (use-package orderless
;;   :ensure t
;;   :custom
;;   (completion-styles '(orderless basic))
;;   (completion-category-overrides '((file (styles basic partial-completion)))))

;; ;; END OF VERTICO CONFIG
;; ;;;;;;;;;;;;;;;;;;;;

;; ;; Enable rich annotations using the Marginalia package
;; (use-package marginalia
;;   ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
;;   ;; available in the *Completions* buffer, add it to the
;;   ;; `completion-list-mode-map'.
;;   :bind (:map minibuffer-local-map
;; 			  ("M-A" . marginalia-cycle))

;;   ;; The :init section is always executed.
;;   :init

;;   ;; Marginalia must be actived in the :init section of use-package such that
;;   ;; the mode gets enabled right away. Note that this forces loading the
;;   ;; package.
;;   (marginalia-mode))

;; (use-package nerd-icons-completion
;;   :after marginalia
;;   :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
;;   :config
;;   (nerd-icons-completion-mode))

;; (use-package consult

;; ;; Example configuration for Consult
;; (use-package consult
;;   ;; Replace bindings. Lazily loaded due by `use-package'.
;;   :bind (;; C-c bindings in `mode-specific-map'
;;          ("C-c M-x" . consult-mode-command)
;;          ("C-c h" . consult-history)
;;          ("C-c k" . consult-kmacro)
;;          ("C-c i" . consult-info)
;;          ([remap Info-search] . consult-info)
;;          ;; C-x bindings in `ctl-x-map'
;;          ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
;;          ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
;;          ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
;;          ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
;;          ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
;; 		 ;;         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
;;          ;; Custom M-# bindings for fast register access
;;          ("M-#" . consult-register-load)
;;          ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
;;          ("C-M-#" . consult-register)
;;          ;; Other custom bindings
;;          ("M-y" . consult-yank-pop)                ;; orig. yank-pop
;;          ;; M-g bindings in `goto-map'
;; 		 ;;         ("M-g e" . consult-compile-error)
;; 		 ;;         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
;;          ("M-g" . consult-goto-line)             ;; orig. goto-line
;;          ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
;;          ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
;;          ;; ("M-g m" . consult-mark)
;;          ;; ("M-g k" . consult-global-mark)
;;          ;; ("M-g i" . consult-imenu)
;;          ;; ("M-g I" . consult-imenu-multi)
;;          ;; M-s bindings in `search-map'
;;          ("M-s d" . consult-find)
;;          ("M-s D" . consult-locate)
;;          ("M-s g" . consult-grep)
;;          ("M-s G" . consult-git-grep)
;;          ("M-s r" . consult-ripgrep)
;;          ("M-s l" . consult-line)
;;          ("M-s L" . consult-line-multi)
;;          ("M-s k" . consult-keep-lines)
;;          ("M-s u" . consult-focus-lines)
;;          ;; Isearch integration
;;          ("M-s e" . consult-isearch-history)
;;          :map isearch-mode-map
;;          ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
;;          ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
;;          ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
;;          ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
;;          ;; Minibuffer history
;;          :map minibuffer-local-map
;;          ("M-s" . consult-history)                 ;; orig. next-matching-history-element
;;          ("M-r" . consult-history))                ;; orig. previous-matching-history-element

;;   ;; Enable automatic preview at point in the *Completions* buffer. This is
;;   ;; relevant when you use the default completion UI.
;;   :hook (completion-list-mode . consult-preview-at-point-mode)

;;   ;; The :init configuration is always executed (Not lazy)
;;   :init

;;   ;; Optionally configure the register formatting. This improves the register
;;   ;; preview for `consult-register', `consult-register-load',
;;   ;; `consult-register-store' and the Emacs built-ins.
;;   (setq register-preview-delay 0.5
;;         register-preview-function #'consult-register-format)

;;   ;; Optionally tweak the register preview window.
;;   ;; This adds thin lines, sorting and hides the mode line of the window.
;;   (advice-add #'register-preview :override #'consult-register-window)

;;   ;; Use Consult to select xref locations with preview
;;   (setq xref-show-xrefs-function #'consult-xref
;;         xref-show-definitions-function #'consult-xref)

;;   ;; Configure other variables and modes in the :config section,
;;   ;; after lazily loading the package.
;;   :config

;;   ;; Optionally configure preview. The default value
;;   ;; is 'any, such that any key triggers the preview.
;;   ;; (setq consult-preview-key 'any)
;;   ;; (setq consult-preview-key "M-.")
;;   ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
;;   ;; For some commands and buffer sources it is useful to configure the
;;   ;; :preview-key on a per-command basis using the `consult-customize' macro.
;;   (consult-customize
;;    consult-theme :preview-key '(:debounce 0.2 any)
;;    consult-ripgrep consult-git-grep consult-grep
;;    consult-bookmark consult-recent-file consult-xref
;;    consult--source-bookmark consult--source-file-register
;;    consult--source-recent-file consult--source-project-recent-file
;;    ;; :preview-key "M-."
;;    :preview-key '(:debounce 0.4 any))

;;   ;; Optionally configure the narrowing key.
;;   ;; Both < and C-+ work reasonably well.
;;   ;; doesn't work (?)
;;   (setq consult-narrow-key "<") ;; "C-+"

;;   ;; Optionally make narrowing help available in the minibuffer.
;;   ;; You may want to use `embark-prefix-help-command' or which-key instead.
;;   ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

;;   ;; By default `consult-project-function' uses `project-root' from project.el.
;;   ;; Optionally configure a different project root function.
;;   ;;;; 1. project.el (the default)
;;   ;; (setq consult-project-function #'consult--default-project--function)
;;   ;;;; 2. vc.el (vc-root-dir)
;;   ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;   ;;;; 3. locate-dominating-file
;;   ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
;;   ;;;; 4. projectile.el (projectile-project-root)
;;   ;; (autoload 'projectile-project-root "projectile")
;;   ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
;;   ;;;; 5. No project support
;;   ;; (setq consult-project-function nil)
;;   )


;; This functoin is mostly from: https://gist.github.com/magnars/3292872
(defun goto-line-with-feedback (&optional line)
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive "P")
  (if line
      (goto-line line)
    (unwind-protect
        (progn
          (nlinum-mode 1)
          (goto-line (read-number "Goto line: ")))
      (nlinum-mode -1))))

(use-package nlinum
  :bind
  (("M-g" . goto-line-with-feedback)))

(use-package treemacs)
(use-package treemacs-projectile)
(use-package dockerfile-mode)

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :bind
  (("C-c E" . emojify-insert-emoji)))


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
        (message "Don't bring sudo in here")))))

(global-set-key (kbd "C-c s") 'open-current-file-with-sudo-tramp)

(server-start)

;; finally
(org-roam-dailies-goto-today)
;; ;;; init.el ends here
