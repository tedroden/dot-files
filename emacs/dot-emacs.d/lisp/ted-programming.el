;;; ted-programming.el --- Programming language support -*- lexical-binding: t -*-

;;; Commentary:
;; LSP, tree-sitter, language modes, formatting, and AI completion.

;;; Code:

;;; LSP

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-restart 'ignore)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-apply-edits-after-file-operations nil)
  (setq lsp-file-watch-threshold 5000)
  :hook ((typescript-ts-mode . lsp)
         (yaml-mode . lsp)
         (json-mode . lsp)
         (css-mode . lsp)
         (bash-mode . lsp)
         (sh-mode . lsp))
  :commands lsp
  :config
  (add-hook 'before-save-hook
            (lambda ()
              (when (derived-mode-p 'typescript-ts-mode 'typescript-mode)
                (lsp-organize-imports)
                (lsp-format-buffer)))))

(use-package lsp-pyright
  :ensure t
  :custom
  (lsp-pyright-langserver-command "basedpyright")
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))

(use-package lsp-ui
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc--sideline-pos-y 0)
  (lsp-ui-doc-delay 0.5))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;;; Tree-sitter

(require 'treesit)
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)))

;;; Language modes

(use-package typescript-ts-mode
  :ensure t)

(use-package python
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

;;; Formatting

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

;;; AI completion

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :hook (prog-mode . copilot-mode)
  :custom
  (copilot-node-executable (executable-find "node"))
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion)))

(provide 'ted-programming)
;;; ted-programming.el ends here
