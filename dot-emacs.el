
;; Remember: you can press [F4] to open this file from emacs.
;; (info "(eintr) Top")  ; press control-e from here to get the lisp tutorial


;; turn off a lot of the UI
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) 
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

;; setup custom/personal/etc.
(setq dotfiles-dir "~/.emacs.d/")

(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
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

(setq c-default-style "linux")
(setq-default c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)



(defun tedroden/edit-dot-emacs ()
  (interactive)
  (find-file (concat dotfiles-dir "init.el"))
  )

(global-set-key "\M-g" 'goto-line)	
(global-set-key "\M-_" 'shrink-window)
(global-set-key "\M-+" 'enlarge-window)
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
(setq dired-omit-mode nil)

;; get package stuff ready
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; thanks federico
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; previously, I did `:ensure t` for every `use-package` module
(setq use-package-always-ensure t)

(defun tedroden/no-suspend ()
  (interactive)
  (message "Not suspending frame. You're welcome"))

(global-set-key (kbd "C-z") 'tedroden/no-suspend)

;; just for my chromebook!! emacs can do anything.
(global-set-key (kbd "<deletechar>") 'backward-kill-word)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme related stuff
(use-package doom-themes
 :init
										;(load-theme 'doom-snazzy)
;; (load-theme 'doom-dark+)
 (load-theme 'doom-material)
 ;; (load-theme 'doom-outrun-electric)
 ;; (load-theme 'doom-peacock)
 ;; (load-theme 'doom-molokai)
 ;; (load-theme 'doom-nord)
 ;; (load-theme 'doom-spacegrey)
 ;; (load-theme 'doom-tomorrow-day)
 ;; (load-theme 'doom-tomorrow-night)
 ;; (load-theme 'doom-vibrant)
 )

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-icon t "icons")
  (doom-modeline-height 64 "height")
  (doom-modeline-buffer-encoding nil "don't show UTF-8 everywhere")
  (doom-modeline-icon (display-graphic-p))
  )

(use-package battery
  :ensure t
  :config
  (display-battery-mode))

(use-package time
  :ensure t
  :custom
  (display-time-default-load-average nil "Don't show load average")

  :config
  (display-time-mode))

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
  :hook ((dired-mode . all-the-icons-dired-mode)))

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

(use-package eshell
  :bind  (("C-!" . eshell))
  :config
  (progn
	;; (add-hook 'eshell-mode-hook 'eshell-mode-hook-func)
	(add-hook 'eshell-preoutput-filter-functions
			  'ansi-color-apply)
	;; requires `eshell-git-prompt`
	(eshell-git-prompt-use-theme 'powerline)
	))

;; show eshell right under the current window
(use-package eshell-toggle
  :bind (("C-c e" . eshell-toggle)))

(use-package keyfreq
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

;; (use-package ido
;;   :bind
;;   ( ("C-x C-f" . ido-find-file) ))

(use-package helm
  :bind (
		 ("C-x C-f" . helm-find-files)
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
  :bind (("C-x C-b" . ibuffer))
  :custom
  (ibuffer-never-show-predicates '("*helm") "don't show helm")
  (ibuffer-show-empty-filter-groups nil "Don't show empty groups")
  (ibuffer-saved-filter-groups '(("Buffers"
								  ("Fancy Hands Code" (filename . "code/fh"))
								  ("Dot Files" (filename . "dot-files"))								  
								  ("wlib" (filename . "code/wlib"))
								  ("Emacs" (or (filename . "dot-emacs.el")
											   (filename . "init.el")
											   (name . "\*GNU Emacs\*")
											   (name . "\*scratch\*")
											   (name . "\*Messages\*")
											   ))
								  ("WM" (mode . exwm-mode))
								  ("GIT" (mode . magit-mode))
								  ("Mail" (name . "\*notmuch"))	 
								  ("Org" (mode . org-mode))
								  ("Eshell" (mode . eshell-mode))
								  ("Man" (name . "\*Man"))	 
								  ("z Helm Garbage" (name . "\*helm")) ;; how do i sort this to the bottom?
								  )))
  :init
  (add-hook 'ibuffer-mode-hook
			'(lambda ()
			   (ibuffer-switch-to-saved-filter-groups "Buffers"))))

;;;;

;;;;;; this is useful if pair programming or working on screen
(use-package beacon
  :init
  (beacon-mode t))

(use-package pkgbuild-mode
  :mode
  (("PKGBUILD$" . pkgbuild-mode)))

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

;; https://github.com/krgn/emacs.d/blob/master/config/setup-auto-complete.el
(use-package auto-complete
  :commands auto-complete-mode
  :init
  (progn
	(auto-complete-mode t))
  :config
  (progn 
	; (use-package auto-complete-config)

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
  :hook (c-mode . ac/headers-local)
  :config (defun ac/headers-local()
			(progn
			  (add-to-list 'ac-sources 'ac-source-c-headers)
			  (add-to-list 'ac-sources 'ac-source-c-header-symbols t))))

(use-package mastodon
  :ensure t
  :custom
  (mastodon-instance-url  "https://fosstodon.org"))

(use-package pdf-tools
  :defer 2
  :ensure t)

;; eh?
(use-package symon
  :custom
  (symon-sparkline-type 'boxed "fewer gridlines")
  :config
  (symon-mode))

;;;;
;;;;
;;;;;; ;; load the theme if we're in xwindows or on a mac

(use-package winner
  :init (winner-mode))

;; ?
(use-package diminish
  :defer 5)

;; move buffers around. neato
(use-package buffer-move
  :config
  (global-set-key (kbd "<C-S-up>")     'buf-move-up)
  (global-set-key (kbd "<C-S-down>")   'buf-move-down)
  (global-set-key (kbd "<C-S-left>")   'buf-move-left)
  (global-set-key (kbd "<C-S-right>")  'buf-move-right))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; some EXWM madness

;; (use-package exwm-config
;;   :init
;;   (require 'exwm-config)  
;;   (exwm-config-default)


(use-package exwm

  :custom
  (exwm-workspace-number 4 "default number of workspaces: 4")
  (exwm-workspace-switch-create-limit 4 "max number of workspaces 4")
  :config
  
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  
  ;; (setq exwm-workspace-index-map (lambda (i) (number-to-string (1+ i))))

  (defmacro goto-wm-workspace (num)
	`(lambda ()
	   (interactive)
	   (goto-wm--workspace ,num)))

  
  (defun ted/run-or-raise (buffer-prefix &optional cmd)
	(let ((popped nil))
	  (dolist (buffer (buffer-list))
		(with-current-buffer buffer
		  (if (string-prefix-p buffer-prefix (buffer-name))
			  (setq popped (pop-to-buffer buffer)))))
	  (if (not popped)
			(start-process-shell-command buffer-prefix nil cmd))))
  

  (defun goto-wm-google ()
	(interactive)
	(ted/run-or-raise "Google-chrome" "/usr/bin/google-chrome-stable --force-device-scale-factor=1.5"))

  (defun goto-wm-terminator ()
	(interactive)
	(ted/run-or-raise "Gnome-terminal" "gnome-terminal"))

  ;; untested because I don't have slack on here.
  (defun goto-wm-slack ()
	(interactive)
	(ted/run-or-raise "Slack"))

  (defun goto-wm-next-workspace ()
	"Go to the next workspace if we're under the limit"
	(interactive)
	(let ((num (+ 1 exwm-workspace-current-index)))
	  (if (< num exwm-workspace-switch-create-limit)
		  (exwm-workspace-switch-create num)
		(message "Too many workspaces"))))
  
  (defun goto-wm-prev-workspace ()
	"Go to the prev workspace if it doesn't take us negative"
	(interactive)
	(if (> exwm-workspace-current-index 0)
		(exwm-workspace-switch-create (- exwm-workspace-current-index 1))
	  (message "Already on first workspace")))


  (defun goto-wm--workspace (num)
	"Go to the prev workspace if it doesn't take us negative"
	(interactive)
	(if (> exwm-workspace-switch-create-limit num)
		(exwm-workspace-switch-create num)))
  
  ;; this renames the buffers. I'll probably change this soon
  (defun exwm-rename-buffer ()
	(interactive)
	(exwm-workspace-rename-buffer
	 (concat exwm-class-name ":"
			 (if (<= (length exwm-title) 30) exwm-title
			   (concat (substring exwm-title 0 29))))))
  
  (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook 'exwm-rename-buffer)
  
  ;; the Control-apostrophe now does a lot of window/buffer related stuff
  ;; to make it work a bit more like (the way I use) stumpwm
  (push ?\C-' exwm-input-prefix-keys)

  ;; Visit buffers with specific apps 
  (exwm-input-set-key (kbd "C-' w") #'goto-wm-google)
  (exwm-input-set-key (kbd "C-' c") #'goto-wm-terminator)
  (exwm-input-set-key (kbd "C-' s") #'goto-wm-slack)
  
  ;; split windows
  (exwm-input-set-key (kbd "C-' |") #'split-window-right)
  (exwm-input-set-key (kbd "C-' -") #'split-window-below)

  ;; move focus directionally
  (exwm-input-set-key (kbd "C-' n") #'windmove-down)
  (exwm-input-set-key (kbd "C-' p") #'windmove-up)
  (exwm-input-set-key (kbd "C-' b") #'windmove-left)
  (exwm-input-set-key (kbd "C-' f") #'windmove-right)
  
  ;; move visible buffer directionally
  (exwm-input-set-key (kbd "C-' N") #'buf-move-down)
  (exwm-input-set-key (kbd "C-' P") #'buf-move-up)
  (exwm-input-set-key (kbd "C-' B") #'buf-move-left)
  (exwm-input-set-key (kbd "C-' F") #'buf-move-right)

  ;; close window
  (exwm-input-set-key (kbd "C-' k") #'kill-buffer)

  (exwm-input-set-key (kbd "C-' g n") #'goto-wm-next-workspace)
  (exwm-input-set-key (kbd "C-' g p") #'goto-wm-prev-workspace)

  ;; this is weird due to your boy's first defmarco (above)
  ;; also, I should be able to do this via dolist, right?
  (exwm-input-set-key (kbd "C-' g 0") (goto-wm-workspace 0))
  (exwm-input-set-key (kbd "C-' g 1") (goto-wm-workspace 1))
  (exwm-input-set-key (kbd "C-' g 2") (goto-wm-workspace 2))
  (exwm-input-set-key (kbd "C-' g 3") (goto-wm-workspace 3))

  ;; (dolist  (num '(number-sequence 0 9))
  ;; 	(message (format "format: %d" num)))
  
  ;; in stumpwm "e" pulls up emacs,
  ;; since we're in emacs, let's just assume
  ;; that i want to switch buffers...
  ;; not sure what I "mean" when I do this if i'm already in emacs
  (exwm-input-set-key (kbd "C-' e") #'ibuffer)

  ;; this is pretty much copied out of exwm-config, with some additions
  (setq exwm-input-global-keys
		`(
		  ;; Bind "s-r" to exit char-mode and fullscreen mode.
		  ([?\s-r] . exwm-reset)
		  ;; Bind "s-w" to switch workspace interactively.
		  ([?\s-w] . exwm-workspace-switch)
		  ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
		  ,@(mapcar (lambda (i)
					  `(,(kbd (format "s-%d" i)) .
						(lambda ()
						  (interactive)
						  (exwm-workspace-switch-create ,i))))
					(number-sequence 0 9))
		  
		  ;; always let me get to the emacs file
		  ([f4] . tedroden/edit-dot-emacs) 
		  )
		)

  (setq exwm-input-simulation-keys
		'(([?\C-b] . [left])
		  ([?\C-f] . [right])
		  ([?\C-p] . [up])
		  ([?\C-n] . [down])
		  ([?\C-a] . [home])
		  ([?\C-e] . [end])
		  ([?\M-v] . [prior])
		  ([?\C-v] . [next])
		  ([?\C-d] . [delete])
		  ([?\C-k] . [S-end delete])))

  (exwm-enable)
  )
  

