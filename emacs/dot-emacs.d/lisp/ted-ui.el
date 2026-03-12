;;; ted-ui.el --- UI settings, theme, and frame font scaling -*- lexical-binding: t -*-

;;; Commentary:
;; UI element configuration, Mac appearance, theme, modeline, and frame font scaling.

;;; Code:

;; UI elements are disabled in early-init.el via frame parameters.
;; Explicitly disable modes for terminal/daemon sessions.
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

;; Mac-specific settings
(when (memq window-system '(mac ns))
  (setq ns-command-modifier 'meta)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; Theme
(use-package ayu-theme
  :ensure t
  :config
  (load-theme 'ayu-dark t))

;; GUI-only settings
(when (display-graphic-p)
  (set-frame-font "Monaco 14")

  (use-package doom-modeline
    :init (doom-modeline-mode 1)
    :custom
    (doom-modeline-icon (display-graphic-p) "icons if we're not in a terminal")
    (doom-modeline-battery t)
    (doom-modeline-height 36)
    (doom-modeline-lsp t)
    (doom-modeline-buffer-encoding nil)))

;;; Frame font scaling

(defun ted/increase-frame-font-size ()
  "Increase the font size for the current frame."
  (interactive)
  (let ((size (face-attribute 'default :height (selected-frame))))
    (set-face-attribute 'default (selected-frame) :height (+ size 10))))

(defun ted/decrease-frame-font-size ()
  "Decrease the font size for the current frame."
  (interactive)
  (let ((size (face-attribute 'default :height (selected-frame))))
    (set-face-attribute 'default (selected-frame) :height (- size 10))))

(defun ted/reset-frame-font-size ()
  "Reset the font size for the current frame to default (140)."
  (interactive)
  (set-face-attribute 'default (selected-frame) :height 140))

(global-set-key (kbd "M-+") 'ted/increase-frame-font-size)
(global-set-key (kbd "M-=") 'ted/increase-frame-font-size)
(global-set-key (kbd "M--") 'ted/decrease-frame-font-size)
(global-set-key (kbd "M-0") 'ted/reset-frame-font-size)

(provide 'ted-ui)
;;; ted-ui.el ends here
