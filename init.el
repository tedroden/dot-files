;; -*- coding: utf-8 -*-
;;
;; This is Ted Roden's .emacs file.
;; http://tedroden.com
;; tedroden@gmail.com
;; Let me know if you use it.
;;
;; * Installation *
;;
;; 1) Rename this directory to ~/.emacs.d
;; 2) If you have an existing ~/.emacs file, move it out of the way.
;; 3) That's it.
;;
;; * Requirements *
;; Most modern emacs should have everything this needs.
;; 
;; I provide a ton of extra stuff in the lisp/ folder
;; I didn't write most of that stuff, 
;; They are all licensed either GPL or BSD, respect that
;; 
;; * Misc *

;; NO WARRANTY, assume a GPL or BSD license.

;; all the requires at the top. 
(add-to-list 'load-path "~/.emacs.d/lisp")

; (message load-path)
(require 'textexpander-sync)
(require 'osx-plist)
(require 'color-theme)
(require 'gnus)
(require 'smtpmail)


(require 'wdired)
(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)

(if (string= system-type 'darwin)
	(progn (require 'utf-8m)
		   (set-file-name-coding-system 'utf-8m))
	 nil)

(require 'php-mode)

(require 'iswitchb)

(iswitchb-mode t)
(add-to-list 'iswitchb-buffer-ignore "^ ")
;(add-to-list 'iswitchb-buffer-ignore "*Minibuf*")
(add-to-list 'iswitchb-buffer-ignore "*Minibuf*")
(add-to-list 'iswitchb-buffer-ignore "^[tT][aA][gG][sS]$")

;; colors 
(color-theme-initialize)

; used helps on iterm on remote system.
; (global-set-key (kbd "ESC C-d") 'backward-kill-word)

;; fix backspace
(normal-erase-is-backspace-mode 1)

;; turn off the toolbar (set to t for on)
(tool-bar-mode -1)

;; how many lines should we scroll when reaching the end of the buffer?
(setq scroll-step 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; what do you think this does?
(setq-default tab-width 4)

;; when emacs asks us yes or no questions. accept y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Show the time in the minibuffer 
(display-time) 

(require 'uniquify)

;; Control-space starts a mark.
;; this is helpful for:
;;  - control-space (select a region) 
;;  - then control-semicolon (comment that region)
;;  - this took me forever to figure out
(transient-mark-mode t)

;; transparency, translucency, opacity, etc.
(modify-frame-parameters (selected-frame) '((alpha . 90)))

(setq mac-command-modifier 'meta) 

;; set up some key bindings (setq default-tab-width 4)
(global-set-key "\M-g" 'goto-line)		
(global-set-key "\M-_" 'shrink-window)
(global-set-key "\M-+" 'enlarge-window)

(global-set-key "\C-w" 'kill-region)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

; (global-set-key "\M-;" 'comment-dwim)  

(defun teds-code-setup ()
  (interactive)
   (highlight-phrase "FIXME" 'hi-yellow)
   (highlight-phrase "Fixme" 'hi-yellow)
   (highlight-phrase "fixme" 'hi-yellow)
   (highlight-phrase "TODO" 'hi-yellow)
   (highlight-phrase "Todo" 'hi-yellow)
   (highlight-phrase "todo" 'hi-yellow))


(defun teds-code-tab-thing () 
  (interactive)
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36))
   (setq tab-width 4)
   (setq c-basic-offset 4)
   (c-set-offset 'substatement-open 0)
   (setq indent-tabs-mode t)
   (teds-code-setup))

(defun teds-lisp-thing ()
  (interactive)
  (teds-code-setup))

(add-hook 'php-mode-hook 'teds-code-tab-thing)
(add-hook 'c-mode-hook 'teds-code-tab-thing)
(add-hook 'objc-mode-hook 'teds-code-tab-thing)
(add-hook 'lisp-mode-hook 'teds-lisp-thing)
(add-hook 'python-mode-hook 'teds-code-setup)
(add-hook 'emacs-lisp-mode-hook 'teds-code-setup)
(add-hook 'js2-mode-hook 'teds-code-setup)

;; paren-pair mode
(require 'paren-pair)

;; do a bunch of paren-pair stuff modes in a row.
;; turn on paren pair mode (insert "()" when you type "(")
(add-hook 'emacs-lisp-mode-hook 'turn-on-paren-pair-mode)
(add-hook 'muse-mode-hook 'turn-on-paren-pair-mode)
(add-hook 'php-mode-hook 'turn-on-paren-pair-mode)
(add-hook 'javascript-mode-hook 'turn-on-paren-pair-mode)

;; org
;; if you want to save a whole lot of loading time, 
;; git rid of this line
(load-file "~/.emacs.d/lisp/init-muse.el")

;; C-x p goes to the previous window (opposite of C-x o)
(defun prev-window () 
  "go to previous window"
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x p") 'prev-window)


(defun growl-init ()
  "init growl"
  (interactive)
  (do-applescript "tell application \"GrowlHelperApp\"
                   set the allNotificationsList to {\"elisp growl\"}
                   set the enabledNotificationsList to {\"elisp growl\"}
                   register as application \"elisp Growl\" all notifications allNotificationsList default notifications enabledNotificationsList icon of application \"Emacs\"
                   end tell"))

(defun growl-msg (title msg)
  "Growl"
  (interactive)
  (list
   (growl-init)
   (do-applescript (concat "tell application \"GrowlHelperApp\"
                             notify with name \"elisp growl\" title \"" title "\" description \"" msg "\" application name \"elisp Growl\"
                             end tell"))))





;; want lisp?
; (require 'slime)
(setq inferior-lisp-program "/opt/local/bin/sbcl")


(load "~/.emacs.d/lisp/nxml/rng-auto.el")
  (setq auto-mode-alist
        (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
	      auto-mode-alist))

(defun log-message (str)
  "Log the selected region to the log-it file"
  (let ((coding-system-for-write 'utf-8))
	(with-temp-file (expand-file-name "~/.logit.txt")
	  (insert-file-contents (expand-file-name "~/.logit.txt") nil nil nil t)
	  (insert (concat (current-time-string) " - " str "\n")))))

(defun did-something ()
  "Tell us what you did. Kind of a private twitter."
  (interactive)
  (setq what (read-from-minibuffer "What did you do? "))
  (log-message what)
  (message (concat "Logged: " what)))

(global-set-key (kbd "C-c \"") 'did-something)


(load-file "~/.emacs.d/lisp/twit.el")

;; google-region
(defun google-region (&optional flags)
  "Google the selected region"
  (interactive)
  (let ((query (buffer-substring (region-beginning) (region-end))))
	(browse-url (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" query))))

;; press control-c g to google the selected region
(global-set-key (kbd "C-c g") 'google-region)


;; abbrev FIXME:
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(if (file-exists-p abbrev-file-name)
	(progn (load-file abbrev-file-name) 
		   (setq save-abbrevs t)
		   (quietly-read-abbrev-file) 
		   (setq default-abbrev-mode t))
  nil)


;; bbdb
(add-to-list 'load-path "~/.emacs.d/lisp/bbdb")
(require 'bbdb)
(require 'contacts-bbdb)
(bbdb-initialize 'message)

;; color theme
(load-library "color-theme-colorful-obsolescence")
(load-library "color-theme-hober2")
(load-library "color-theme-tango")
; (color-theme-colorful-obsolescence)
(color-theme-hober2)

;; w3m
(add-to-list 'load-path "~/.emacs.d/lisp/w3m")
(if (= emacs-major-version 23)
	(require 'w3m-load)
	(require 'w3m)
    )

(add-to-list 'load-path "~/.emacs.d/lisp/org")
(setq org-fontify-done-headline t)
(setq org-return-follows-link t) ;; needs to be set BEFORE org is loaded
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-todo-keywords
	  '((sequence "TODO" "FOLLOWUP" "|" "DONE" "DELEGATED")))

(setq org-hide-leading-stars t)
;; open an email message from a link in org-mode
(org-add-link-type "message" 'teds-org-mail-message-open)

(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)



(defun teds-org-mail-message-open (message-id)
  "Visit the email message with the given Message-ID in Mail.app (or registered mail client)."
  (browse-url (concat "message:" message-id)))


(add-to-list 'load-path "~/.emacs.d/lisp/remember")
(require 'remember)
(global-set-key "\M-3" 'remember)


(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 4)
(setq js2-use-font-lock-faces t)

;; for yatk js files (!)
(add-to-list 'auto-mode-alist '("\\.yjs$" . js2-mode))


(add-to-list 'auto-mode-alist '("\\.jsp$" . html-mode))


;;;;;;;;;;;
;; I like to write long email in emacs
;; this handles linebreaks pretty well
(add-hook 'mail-mode-hook
		  (function (lambda ()
					  (longlines-mode 1))))

;; turn off long lines before sending
(add-hook 'mail-send-hook
		  (function (lambda ()
					  (longlines-mode nil))))


(add-hook 'eshell-mode-hook
		  '(lambda nil
			 (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:" (expand-file-name "~/.sw/bin")))))

;; git!
;; (require 'git-emacs)
;; (require 'git-modeline)
(require 'git-blame)

;; you may want to change these!
(setq python-mode-hook
	  '(lambda () (progn
					(set-variable 'python-indent 8)
					(set-variable 'indent-tabs-mode t) )))

(add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

;; emacs will auto save custom stuff in this file 
;; i also put anything in there that applies only to me.
;; save the custom stuff in that file
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
	(load custom-file) nil)

(setq ring-bell-function '(lambda () (message "*beep*")))



