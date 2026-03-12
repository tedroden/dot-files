;;; ted-tools.el --- Tools, navigation, and project management -*- lexical-binding: t -*-

;;; Commentary:
;; Magit, projectile, ivy/counsel, company, terminals, and other tools.

;;; Code:

;;; Project management

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/code")))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode)
  :bind
  (("C-c k" . 'counsel-projectile-rg)
   ("M-p" . 'counsel-projectile-find-file)
   ("C-c 4 f" . 'projectile-find-file-other-window)
   ("C-c C-f" . 'counsel-projectile-find-file)))

;;; Search and file navigation

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
  (setq counsel-find-file-ignore-regexp "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)")
  :bind
  (("C-x b" . 'ivy-switch-buffer)
   ("C-x C-b" . 'ivy-switch-buffer)
   ("M-x" . 'counsel-M-x)
   ("C-x C-f" . 'counsel-find-file)
   ("C-x d" . 'counsel-dired)
   ("C-h f" . 'counsel-describe-function)
   ("C-h v" . 'counsel-describe-variable)
   ("M-y" . 'counsel-yank-pop)))

(use-package ivy-rich
  :ensure t
  :after ivy)

;;; Git integration

(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status))
  :custom
  (git-commit-major-mode 'markdown-mode)
  (magit-save-repository-buffers 'dontask))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package diff-hl
  :ensure t
  :hook (prog-mode . diff-hl-mode)
  :config
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode))

;;; Code completion

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.5)
  :bind (:map company-active-map ("<enter>" . company-complete-selection)))

;;; Navigation and editing

(use-package switch-window
  :bind (("M-o" . switch-window))
  :custom
  (switch-window-shortcut-style 'qwerty))

(use-package avy
  :bind
  ("C-/" . 'avy-goto-char-2)
  ("M-j" . 'avy-goto-char-timer))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package saveplace
  :init (save-place-mode 1)
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-limit nil)))

(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode))

;;; Markdown

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :bind
  (:map markdown-mode-map
        ("<tab>" . markdown-cycle)
        ("S-<tab>" . markdown-shifttab))
  :init
  (setq markdown-hide-markup-in-view-modes t)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-spaces-after-code-fence 0)
  :config
  (add-hook 'markdown-mode-hook #'font-lock-mode))

;;; Buffer management

(use-package ibuffer
  :bind ("C-c b" . ibuffer))

(use-package ibuffer-projectile
  :ensure t
  :custom
  (ibuffer-projectile-prefix "")
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;;; Tree sidebar

(use-package treemacs :defer t)
(use-package treemacs-projectile :defer t)

;;; Terminals

(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

(use-package eat :ensure t :defer t)
(use-package vterm :ensure t :defer t)

;;; Misc

(use-package nginx-mode :defer t)

(defun ted/safe-refresh-font-lock ()
  "Refresh font lock safely using font-lock-ensure."
  (interactive)
  (when (fboundp 'font-lock-ensure)
    (font-lock-ensure)))

;; Daily notes (requires counsel)
(require 'noted)

(provide 'ted-tools)
;;; ted-tools.el ends here
