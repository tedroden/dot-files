
;; Remember: you can press [F4] to open this file from emacs.
;; (info "(eintr) Top")  ; press control-e from here to get the lisp tutorial


; If you aren't using :commands, :bind, :bind*, :bind-keymap, :bind-keymap*, :mode, :interpreter, or :hook (all of which imply :defer;
																											  
;; turn off a lot of the UI
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) 
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

;; setup custom/personal/etc.
(setq dotfiles-dir "~/.emacs.d/")
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq custom-file (concat dotfiles-dir "custom.el"))
(setq personal-file (concat dotfiles-dir "personal.el"))
(dolist (f (list custom-file personal-file))
  (if (file-exists-p f)
	  (progn (load f)
		 (message (concat "Loaded " f)))
    nil))

;; all "yes" or "no" questions should be y/n
(fset 'yes-or-no-p 'y-or-n-p)

(global-hi-lock-mode 1)

(defun tedroden/edit-dot-emacs ()
  (interactive)
  (find-file (concat dotfiles-dir "init.el"))
  )

(global-set-key "\M-g" 'goto-line)	
(global-set-key "\M-_" 'shrink-window)
(global-set-key "\M-+" 'enlarge-window)
; (global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "C-x p") 'tedroden/prev-window)
(global-set-key [f4] 'tedroden/edit-dot-emacs)
(global-set-key (kbd "C-c q") 'ff-find-other-file)
(global-set-key (kbd "C-c |") 'split-window-right)
(global-set-key (kbd "C-c -") 'split-window-below)


;; Command should be META on the mac
(setq ns-command-modifier 'meta)

;; confirm on exit
(setq confirm-kill-emacs 'yes-or-no-p)

;;;; Want line numbers?
;; (global-linum-mode t) ;; Line numbersthis is good for teaching, but i don't generally want it.

;;;; highlight the current line?
;; (global-hl-line-mode nil)

;; column number (lives in mode line)
(column-number-mode t)

;;;; show hidden files in dired?
; (setq dired-omit-mode nil)

;; get package stuff ready
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; not sure?
(eval-when-compile
  (require 'use-package))

(defun tedroden/no-suspend ()
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
 (load-theme 'doom-snazzy))


(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;;; end theme related
;;;;;;;;;;;;;;;;;;;;;;


;; great for quickly switching windows if you've got more than 2
;; (use-package ace-window
;;   :ensure t
;;   :bind ("M-o" . ace-window))
(use-package switch-window
  :ensure t
  :bind (("M-o" . switch-window))
  :custom
  (switch-window-shortcut-style 'qwerty "use letters instead of numbers"))

;; super cool search if you can see where you want to go.
(use-package avy
  :ensure t
  :bind ("C-/" .'avy-goto-char-2))

;; The package is "python" but the mode is "python-mode":
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;;;;
;;;;(use-package undo-tree;
;;;  :defer t
;;;;  :ensure t
;;;;  :diminish undo-tree-mode
;;;;  :config
;;;;  (progn
;;;;    (global-undo-tree-mode)
;;;;    (setq undo-tree-visualizer-timestamps t)
;;;;    (setq undo-tree-visualizer-diff t)))
;;;;

(use-package artbollocks-mode
  :defer 4 ;; we don't actually need this, so don't load it for a while
  :config
  (progn
    (setq artbollocks-weasel-words-regex
          (concat "\\b" (regexp-opt
                         '("one of the"
                           "should"
                           "just"
                           "sort of"
                           "a lot"
                           "probably"
                           "maybe"
                           "perhaps"
                           "I think"
                           "really"
                           "pretty"
                           "nice"
                           "action"
                           "utilize"
                           "leverage") t) "\\b"))
    ;; Don't show the art critic words, or at least until I figure
    ;; out my own jargon
    (setq artbollocks-jargon nil)))

;;;; show icons in dired! (requires all-theicons-dired)
(use-package all-the-icons-dired
  :ensure t
  :hook ((dired-mode . all-the-icons-dired-mode)))

;;;;
;;;;;; magit setup. Is this right?
(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status)))

(use-package rainbow-mode
  :ensure t
  :hook ((css-mode . rainbow-mode)
		 (sass-mode . rainbow-mode)))



(defun tedroden/code-setup ()
  (interactive)
  (highlight-phrase "FIXME" 'hi-yellow)
  (highlight-phrase "Fixme" 'hi-yellow)
  (highlight-phrase "fixme" 'hi-yellow)
  (highlight-phrase "TODO" 'hi-yellow)
  (highlight-phrase "Todo" 'hi-yellow)
  (highlight-phrase "todo" 'hi-yellow)
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

;;;;
;;;;;; set up some key bindings (setq default-tab-width 4)

;;;;
;;;;;; ;; (use-package org-notmuch)
;;;;;; (use-package org
;;;;;;   :bind (("C-c a" . org-agenda)
;;;;;; 	 ("C-c c" . org-capture)
;;;;;; 	 ("C-c l" . org-store-link))
;;;;;;   :config
;;;;;;   (progn
;;;;;;     (setq org-default-notes-file "~/Dropbox/org/todo.org")
;;;;;;     (setq org-agenda-files '("~/Dropbox/org"))
;;;;
;;;;;;     (setq org-catch-invisible-edits 'show-and-error)    
;;;;;;     (setq org-log-done 'time)
;;;;;;     (setq org-log-into-drawer t)
;;;;;;     (setq org-clock-out-when-done t)
;;;;
;;;;
;;;;;;     (setq org-refile-targets
;;;;;; 	  '((nil :maxlevel . 1)
;;;;;; 	    (org-agenda-files :maxlevel . 1)))    
;;;;;;     ))
;;;;

;;;;
;;;;; setup the path
;;;;(defun eshell-mode-hook-func ()
;;;;  (message "setting this up.")
;;;;  (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env)))
;;;;
(use-package eshell
  :bind  (("C-!" . eshell))
  :config
  (progn
	;; (add-hook 'eshell-mode-hook 'eshell-mode-hook-func)
	(add-hook 'eshell-preoutput-filter-functions
			  'ansi-color-apply)
	(eshell-git-prompt-use-theme 'powerline)
	))

;; show eshell right under the current window
(use-package eshell-toggle
  :ensure t
  :bind (("C-c e" . eshell-toggle)))

;;;;
;;;;;; start the server
;;;;;; FIXME: can we check to see if it's running first?
;;;;;; (server-start)
;;;;
;;;;
;;;;;; this *should* make it so we don't open new frames

;;;;
;;;;;; (setq c-default-style "linux")
;;;;(setq-default c-basic-offset 4
;;;;			  tab-width 4
;;;;			  indent-tabs-mode t)
;;;;
;;;;

(use-package keyfreq
 :ensure t
 :config
 (progn 
   (keyfreq-mode 1)
   (keyfreq-autosave-mode 1)
   (setq keyfreq-excluded-commands
     '(self-insert-command
       abort-recursive-edit
       org-self-insert-command
       forward-char
       backward-char
       previous-line
       next-line))))

(use-package helm
  :ensure t
  :bind (("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-mini)
	 ("M-x" . helm-M-x)
	 ("C-h a" . helm-apropos)
	 ("M-y". helm-show-kill-ring) ;; eh...
	 ("M-i" . helm-swoop-without-pre-input)
	 :map helm-map
	 ("<tab>" . helm-execute-persistent-action)
	 )
  :config (progn
	    (setq helm-ff-file-name-history-use-recentf t)
	    (setq helm-buffers-fuzzy-matching t)
	    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
	    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
	    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z	    
		(helm-mode 1)))

;;;;
;;;;
(use-package ibuffer
  :ensure t
  :config
  (progn
	(setq ibuffer-show-empty-filter-groups nil)
	(setq ibuffer-saved-filter-groups
		  '(("Buffers"
			 ("Fancy Hands Code" (filename . "code/fh"))
			 ("wlib" (filename . "code/wlib"))
			 ("Emacs" (or (filename . "dot-emacs.el")
						  (name . "\*GNU Emacs\*")
						  (name . "\*scratch\*")
						  (name . "\*Messages\*")
						  ))
			 ("Mail" (name . "\*notmuch"))	 
			 ("Org" (mode . org-mode))
			 ("Eshell" (mode . eshell-mode))
			 ("Man" (name . "\*Man"))	 
			 ("z Helm Garbage" (name . "\*helm")) ;; how do i sort this to the bottom?
			 ))))
  :init
  (add-hook 'ibuffer-mode-hook
			'(lambda ()
			   (ibuffer-switch-to-saved-filter-groups "Buffers"))))

;;;;

;;;;;; this is useful if pair programming or working on screen
(use-package beacon
  :ensure t
  :init
  (beacon-mode t))

(use-package pkgbuild-mode
  :ensure t
  :mode
  (("PKGBUILD$" . pkgbuild-mode)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package js2-mode
  :ensure t
  :mode (("\\.js$" . js2-mode))
  :interpreter ("node" . js2-mode))

;; put the cursor where it was last time you visited a file
(use-package saveplace
  :ensure t
  :init (save-place-mode 1)
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-limit nil)))

(use-package rjsx-mode
  :ensure t
  :defer t)

;; https://github.com/krgn/emacs.d/blob/master/config/setup-auto-complete.el
(use-package auto-complete
  :ensure t
  :commands auto-complete-mode
  :init
  (progn
	(auto-complete-mode t))
  :config
  (progn 
	(use-package auto-complete-config)

	(ac-set-trigger-key "TAB")
	(ac-config-default)

	(setq ac-delay 0.02)
	(setq ac-use-menu-map t)
	(setq ac-menu-height 50)
	(setq ac-use-quick-help nil) 
	(setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
	(setq ac-ignore-case nil)
	(setq ac-dwim  t)
	(setq ac-fuzzy-enable t)
	))

;;
(use-package ac-c-headers
  :ensure t
  :hook (c-mode . ac/headers-local)
  :config (defun ac/headers-local()
			(progn
			  (add-to-list 'ac-sources 'ac-source-c-headers)
			  (add-to-list 'ac-sources 'ac-source-c-header-symbols t))))

;;;;
;;;;
;;;;;; ;; load the theme if we're in xwindows or on a mac


;;;;
;;;;
;;;;
;;;;
;;;;
(require 'exwm)
(require 'exwm-config)
(exwm-config-default)

(defun exwm-rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-class-name ":"
           (if (<= (length exwm-title) 30) exwm-title
             (concat (substring exwm-title 0 29))))))

(add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
(add-hook 'exwm-update-title-hook 'exwm-rename-buffer)

(require 'exwm-systemtray)
(exwm-systemtray-enable)
