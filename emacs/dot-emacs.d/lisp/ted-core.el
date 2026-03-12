;;; ted-core.el --- Core editing settings and utility functions -*- lexical-binding: t -*-

;;; Commentary:
;; Core Emacs settings: editing defaults, global keybindings, and utility functions.

;;; Code:

;; All "yes" or "no" questions should be y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Performance settings
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Automatically follow symlinks to git-controlled files without prompting
(setq vc-follow-symlinks t)

;; Core editing settings
(setq-default c-default-style "k&r")
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default typescript-ts-mode-indent-offset 4)
(column-number-mode t)

;; Startup settings
(setq initial-major-mode 'fundamental-mode)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(transient-mark-mode 1)
(setq create-lockfiles nil)

;; Replace deprecated font-lock-fontify-buffer with font-lock-ensure
(when (fboundp 'advice-add)
  (advice-add 'font-lock-fontify-buffer :around
              (lambda (orig-fun &rest args)
                (if (fboundp 'font-lock-ensure)
                    (apply 'font-lock-ensure args)
                  (apply orig-fun args)))))

;;; Utility functions

(defun ted/kill-region-or-backward-word ()
  "Kill region if active, otherwise kill the previous word."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun ted/edit-dot-emacs ()
  "Quickly edit my dot Emacs file."
  (interactive)
  (let ((dot-emacs (expand-file-name "~/code/dot-files/emacs/dot-emacs.d/init.el")))
    (unless (file-exists-p dot-emacs)
      (setq dot-emacs (concat user-emacs-directory "init.el")))
    (find-file dot-emacs)))

(defun ok-goto-line (line)
  "Go to the specified LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode 1)
        (let ((num (read-number "Goto line: ")))
          (ok-goto-line num)))
    (display-line-numbers-mode -1)))

(defun ted/insert-date ()
  "Insert current date in format YYYY-MM-DD at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun ted/insert-time ()
  "Insert current time in format HH:MMam/pm at point."
  (interactive)
  (insert (format-time-string "%I:%M%p")))

;;; Global keybindings

(global-set-key (kbd "C-c |") 'split-window-right)
(global-set-key (kbd "C-c -") 'split-window-below)
(global-set-key (kbd "C-c r") 'query-replace)
(global-set-key (kbd "C-c s") 'ispell-word)
(global-set-key (kbd "C-z") (lambda () (interactive) (message "Not suspending frame.")))
(global-set-key [f4] 'ted/edit-dot-emacs)
(global-set-key (kbd "M-g") 'goto-line-with-feedback)
(global-set-key (kbd "C-c P") 'package-list-packages)
(global-set-key (kbd "C-w") 'ted/kill-region-or-backward-word)
(global-set-key (kbd "C-c D") 'ted/insert-date)
(global-set-key (kbd "C-c T") 'ted/insert-time)

(provide 'ted-core)
;;; ted-core.el ends here
