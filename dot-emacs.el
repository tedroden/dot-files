;; Rethinking everything

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) 
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

(setq dotfiles-dir "~/.emacs.d/")
(setq lisp-dir (concat dotfiles-dir "lisp"))
(add-to-list 'load-path lisp-dir)

; (info "(eintr) Top") 

;; FIXME: add artbollocks-mode,
;; FIXME: flyspell to markdown mode

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

; (set-default-font "Hack 14")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(eval-when-compile
  (require 'use-package))

;; ;; FIXME: migrate all this to use package?
;; ;; a list of packages
;; (defvar tedroden/packages
;;   '(auto-complete
;;     artbollocks-mode
;;     helm
;;     helm-swoop
;; ;;    helm-flx
;;     visual-fill-column
;;     ;; ac-ispell
;;     ;; ac-js2
;;     ;; go-mode
;;     ;; go-autocomplete
;;     magit
;;     markdown-mode
;;     expand-region
;;     ;; flx-ido
;;     ace-window
;;     ;; rainbow-blocks
;;     rainbow-delimiters
;;     rainbow-mode ;; colorify hex colors in css, etc.
;;     use-package
;; ;;    smooth-scroll
;;     keyfreq
;;     js2-mode ;; still the best javascript mode as far as I can tell
;;     beacon ;; good for when you're skipping around windows
;;     paradox ;; package managerrment
;; ;    base16-theme ;; theme
;;     web-mode ;; handles django templates (and others)
;; ;    twilight-bright-theme
    
;;     )
;;   "Stuff I like")


;; (if (file-exists-p package-user-dir)
;;     nil
;;   (package-refresh-contents))

;; ;; i'm fairly sure that use-package can handle this...
;; (dolist (p tedroden/packages)
;;   (when (not (package-installed-p p))
;; 	(package-install p)))

(defun tedroden/no-suspend ()
  (interactive)
  (message "Not suspending frame. You're welcome"))

(global-set-key (kbd "C-z") 'tedroden/no-suspend)

;; just for my chromebook!! emacs can do anything.
(global-set-key (kbd "<deletechar>") 'backward-kill-word)

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package avy
  :ensure t  
  :config
  (global-set-key (kbd "C-/") 'avy-goto-char-2))




;; (use-package visual-fill-column-mode
;;   :init
;;   (visual-fill-column-mode))

;; (use-package beacon-mode
;;   :init
;;   (beacon-mode t))

; (require 'use-package)
;; (use-package smooth-scroll
;;   :config
;;   (smooth-scroll-mode 1)
;;   (setq smooth-scroll/vscroll-step-size 5)
;;   )


;; The package is "python" but the mode is "python-mode":
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package undo-tree
  :defer t
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package artbollocks-mode
  :defer t
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



					; (load-theme 'base16-twilight-dark))

;; I go back and forth on these...
;; (global-linum-mode t) ;; this is good for teaching, but i don't generally want it.
;  (global-hl-line-mode nil)
(column-number-mode t)



;; auto complete setup. Is this right?
; (require 'auto-complete-config)
; (add-to-list 'ac-dictionary-directories (concat lisp-dir "ac-dict"))
;; (ac-config-default)

(setq dired-omit-mode nil)

;; magit setup. Is this right?
(use-package magit
  :ensure
  :config
  (progn
    (global-set-key (kbd "C-c m") 'magit-status)    
    (custom-set-variables
    '(magit-git-global-arguments
     (quote
      ("--no-pager" "--literal-pathspecs" "-c" "core.preloadindex=true" "-c" "color.ui=auto"))))))

; (setq magit-dir (concat dotfiles-dir "external/magit/"))
; (add-to-list 'load-path magit-dir)




(global-hi-lock-mode 1)	  
(defun tedroden/code-setup ()
  (interactive)
  (highlight-phrase "FIXME" 'hi-yellow)
  (highlight-phrase "Fixme" 'hi-yellow)
  (highlight-phrase "fixme" 'hi-yellow)
  ;; (highlight-phrase "TODO" 'hi-yellow)
  ;; (highlight-phrase "Todo" 'hi-yellow)
  ;; (highlight-phrase "todo" 'hi-yellow)
  )

;; (defun tedroden/writer-mode ()
;;   (interactive)
;;   (markdown-mode)
;;   (visual-line-mode)
;;   (visual-fill-column-mode))

(defun tedroden/writer-mode ()
  (interactive)
  (message "not loading writer-mode"))

(add-hook 'prog-mode-hook  'tedroden/code-setup)
(add-hook 'text-mode-hook  'tedroden/code-setup)

(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'sass-mode-hook 'rainbow-mode)

;; (eval-after-load "auto-complete"
;;   '(progn
;;      (ac-ispell-setup)))

;; (add-hook 'text-mode-hook  'ac-ispell-ac-setup)

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

;; ;; (use-package org-notmuch)
;; (use-package org
;;   :bind (("C-c a" . org-agenda)
;; 	 ("C-c c" . org-capture)
;; 	 ("C-c l" . org-store-link))
;;   :config
;;   (progn
;;     (setq org-default-notes-file "~/Dropbox/org/todo.org")
;;     (setq org-agenda-files '("~/Dropbox/org"))

;;     (setq org-catch-invisible-edits 'show-and-error)    
;;     (setq org-log-done 'time)
;;     (setq org-log-into-drawer t)
;;     (setq org-clock-out-when-done t)


;;     (setq org-refile-targets
;; 	  '((nil :maxlevel . 1)
;; 	    (org-agenda-files :maxlevel . 1)))    
;;     ))

(global-set-key (kbd "C-!") 'eshell)
(global-set-key (kbd "C-c q") 'ff-find-other-file)
(global-set-key (kbd "C-c |") 'split-window-right)
(global-set-key (kbd "C-c -") 'split-window-below)

; setup the path
(defun eshell-mode-hook-func ()
  (message "setting this up.")
  (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env)))

(use-package eshell
  :config
  (progn
    (add-hook 'eshell-mode-hook 'eshell-mode-hook-func)
    (add-hook 'eshell-preoutput-filter-functions
	      'ansi-color-apply)
    ; (eshell-git-prompt-use-theme 'powerline)
    ))
  

;; start the server
;; FIXME: can we check to see if it's running first?
;; (server-start)


;; this *should* make it so we don't open new frames
(setq ns-pop-up-frames nil)

;; (setq c-default-style "linux")
(setq-default c-basic-offset 8
	      tab-width 8
	      indent-tabs-mode t)

;; (use-package ido
;;   :config
;;   (progn
;;     (ido-mode t)
;;     (ido-everywhere t)
;;     (flx-ido-mode t)))


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

;; (use-package counsel
;;     :config
;;   (progn
;;     (global-set-key (kbd "M-x")     'counsel-M-x)
;;     (global-set-key (kbd "C-x b")     'ivy-switch-buffer)
;;     (global-set-key (kbd "C-x C-f") 'counsel-find-file)

;;     ))

;; (use-package helm-config
;;     :config
;;   (progn

;;     (load (concat dotfiles-dir "helm-init.el"))))


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

       
  



; (use-package helm-projectile)
;; maybe?
(add-hook 'eshell-mode-hook
          (lambda ()
              (eshell-cmpl-initialize)))



;; I know these create problems (with live-reload, etc),
;; not sure if disabling it will create new ones... 
(setq create-lockfiles nil)


(use-package ibuffer
  :ensure t
  :config
  (progn
    (setq ibuffer-show-empty-filter-groups nil)
    (setq ibuffer-saved-filter-groups
	  '(("Buffers"
	     ("All This Becomes You" (filename . "_atby"))	 	 
	     ("Fancy Hands Code" (filename . "code/fancyhands"))
	     ("wlib" (filename . "code/wlib"))
	     ("QVP CLI" (filename . "code/qvp-cli"))
	     ("QVP Server" (filename . "code/qvp-server"))
	     ("QVP Web" (filename . "code/qvp-web"))	 
	     ("Emacs" (or (filename . "dot-emacs.el")
			  (name . "\*GNU Emacs\*")
			  (name . "\*scratch\*")
			  (name . "\*Messages\*")
			  ))
	     ("Mail" (name . "\*notmuch"))	 
	     ("Org" (mode . org-mode))
	     ("Eshell" (mode . eshell-mode))
	     ("Man" (name . "\*Man"))	 
	     ("z Helm Garbage" (name . "\*helm"))
	     ))))
  :init
  (add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups "Buffers"))))
  
;; (use-package ibuffer-vc  
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'ibuffer-hook
;;                   (lambda ()
;;                     (ibuffer-vc-set-filter-groups-by-vc-root)
;;                     (unless (eq ibuffer-sorting-mode 'alphabetic)
;;                       (ibuffer-do-sort-by-alphabetic)))))

	 ;; ("Web Dev" (or (mode . html-mode)
	 ;; 		(mode . css-mode)))
	 ;; ("Subversion" (name . "\*svn"))
	 ;; ("Magit" (name . "\*magit"))
	 ;; ("ERC" (mode . erc-mode))
	 ;; ("Help" (or (name . "\*Help\*")
	 ;; 	     (name . "\*Apropos\*")
	 ;; 	     (name . "\*info\*"))))))

;; (use-package markdown-mode
;;   :init
;;   ; (setq markdown-list-indent-width 2)
;;   (artbollocks-mode 1)
;;   (tedroden/writer-mode)
;; 					;(add-hook 'markdown-mode-hook (tedroden/writer-mode)
;;   )

;; (use-package telephone-line-mode
;;   :init
;;   (telephone-line-mode 1))

;; (require 'spaceline-config)
;; (spaceline-spacemacs-theme)

;; (use-package spaceline
;;     :config
;;     (require 'spaceline-config)
;;     (spaceline-spacemacs-theme)
;;     (spaceline-helm-mode)
;; ;;    (spaceline-emacs-theme)
;;     (spaceline-toggle-minor-modes-on))




(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-snazzy))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

; (use-package pony-mode; )
(use-package web-mode
  :ensure t
  :init
  (progn
    (setq web-mode-engines-alist
	  '(("django"    . "\\.html\\'"))))
  :mode
  ((".*fancyhands/templates/.*\\.html\\'" . web-mode)))


(use-package pkgbuild-mode
  :mode
  (("PKGBUILD$" . pkgbuild-mode)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package js2-mode
  :config
  (progn
    (add-hook 'js2-mode-hook 'ac-js2-mode))
  :mode (("\\.js$" . js2-mode))
  :interpreter ("node" . js2-mode))


;; i don't know about this sone
;; (use-package smartparens
;;   :init
;;   (progn
;;     (use-package smartparens-config)
;;     (use-package smartparens-ruby)
;;     (use-package smartparens-html)
;;     (smartparens-global-mode 1)))

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/local/bin/msmtpq"
      mail-specify-envelope-from t
;; needed for debians message.el cf. README.Debian.gz 
      message-sendmail-f-is-evil nil                
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)

;; put the cursor where it was last time you visited a file
(use-package saveplace
  :ensure t
  :init (save-place-mode 1)
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-limit nil)))

(use-package uniquify-files
  :ensure t)

;; ;; load the theme if we're in xwindows or on a mac
(if (member window-system '(mac x ns))
    (progn
      (global-hl-line-mode nil)
;      (load-theme 'doom-dracula)
;      (exec-path-from-shell-initialize)
      ;;  (load-theme 'base16-ocean)
      )
  )

;; (custom-set-variables
;;  '(powerline-default-separator (quote zigzag))
;;  '(powerline-height 20))


;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)                 ; optional
