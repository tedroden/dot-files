;;; init.el --- Emacs configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;
;;
;; Requires Emacs 30+ (due to: `use-package :vc`)
;;
;; add this to your .bashrc or .zshrc

;; export EDITOR="emacsclient -nw"
;;
;;; I'm currently intalling this emacs:
;; brew tap d12frosted/emacs-plus
;; brew install emacs-plus@30 --with-native-comp
;;
;; osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@30/Emacs.app" at POSIX file "/Applications" with properties {name:"Emacs.app"}'
;;

;; DO NOT reinstall, uninstall and install again.
;; Do this: `brew uninstall emacs-plus@30 && brew unlink emacs-plus@30 && rm /Applications/Emacs.app` and reinstall it.



;; macs session through this interface. To test your new configuration, you should:

;;   1. Open a terminal on your system
;;   2. Run: emacs -nw

;;   This will launch Emacs in terminal mode with your new minimal configuration. You should see:
;;   - A message saying "Terminal mode - minimal config loaded. Press C-c L to load full config."
;;   - Minimal interface without org-mode, LSP, or other heavy packages
;;   - Basic editing capabilities

;;   When you're ready to load the full configuration, just press C-c L and it will load all the remaining packages.

;;   To verify the configuration is working correctly, you can check:
;;   1. Startup speed (should be much faster in terminal mode)
;;   2. Basic editing functionality works
;;   3. Press C-c L to load the full config and confirm everything else loads

;; Let me know how it performs for you!

;; Early check for terminal mode
(defvar ted/is-terminal (not (display-graphic-p))
  "True if Emacs is running in terminal mode.")

(defvar ted/full-config-loaded nil
  "Flag to indicate if full configuration has been loaded.")

;; Fast startup for terminal mode
(when ted/is-terminal
  (setq initial-major-mode 'fundamental-mode)
  (setq inhibit-startup-screen t)
  (setq inhibit-splash-screen t)
  (transient-mark-mode 1)
  (setq gc-cons-threshold 50000000)
  (setq read-process-output-max (* 1024 1024))
  (setq confirm-kill-emacs nil) ;; No confirmation in terminal mode
  (setq create-lockfiles nil))

;; Core package setup - minimal for terminal, full for GUI
(require 'package)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("elpa-devel" . 4)
        ("melpa" . 3)
        ("elpa" . 2)
        ("nongnu" . 1)))

;; Initialize package.el
(package-initialize)

;; Force package refresh and install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Configure use-package to auto-install packages
(require 'use-package)
(setq use-package-always-ensure t)

;; Turn off UI elements
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

;; all "yes" or "no" questions should be y/n
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

;; Global keybindings for all modes
(global-set-key (kbd "C-c |") 'split-window-right)
(global-set-key (kbd "C-c -") 'split-window-below)
(global-set-key (kbd "C-c r") 'query-replace)
(global-set-key (kbd "C-z") (lambda () (interactive) (message "Not suspending frame.")))
(global-set-key [f4] 'ted/edit-dot-emacs)
(global-set-key (kbd "M-g") 'goto-line-with-feedback)
;; meta-; for comment uncomment


;; Edit init.el function (essential)
(defun ted/edit-dot-emacs ()
  "Quickly edit my dot Emacs file."
  (interactive)
  ;; I use a symlinked file by default, so try to open the OG file
  (let ((dot-emacs (expand-file-name "~/code/dot-files/emacs/.emacs.d/init.el")))
    ;; if not, just open the standard path
    (unless (file-exists-p dot-emacs)
      (setq dot-emacs (concat user-emacs-directory "init.el")))
    (find-file dot-emacs)))

;; Function to go to a line number with line numbers displayed temporarily
(defun ok-goto-line (line)
  "Go to the specified LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode 1)
        (let ((num (read-number "Goto line: ")))
          (ok-goto-line num)))
    (display-line-numbers-mode -1)))

;; Function to load the full configuration
(defun ted/load-full-config ()
  "Load the full Emacs configuration."
  (interactive)
  (unless ted/full-config-loaded
    (message "Loading full configuration...")
    
    ;; setup custom/personal/etc.
    (setq-default dotfiles-dir (file-truename "~/.emacs.d/")
                  custom-file (concat dotfiles-dir "custom.el")
                  personal-file (concat dotfiles-dir "personal.el"))

    (dolist (f (list custom-file personal-file))
      (if (file-exists-p f)
          (progn (load f)
                 (message (concat "Loaded " f)))
        nil))
    
    ;; Load straight.el bootstrap
    (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name
            "straight/repos/straight.el/bootstrap.el"
            (or (bound-and-true-p straight-base-dir)
                user-emacs-directory)))
          (bootstrap-version 7))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))
    
    ;; Mac-specific settings
    (when (memq window-system '(mac ns))
      (use-package exec-path-from-shell
        :ensure t
        :config
        (exec-path-from-shell-initialize))
      
      (setq ns-command-modifier 'meta)
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . dark)))
    
    ;; UI enhancements
    ;; (use-package catppuccin-theme
    ;;   :config
    ;;   (load-theme 'catppuccin t)
    ;;   :custom
    ;;   (catppuccin-enlarge-headings nil))


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
    
    ;; Programming tools
    (use-package lsp-mode
      :ensure t
      :init
      (setq lsp-keymap-prefix "C-c l")
      (setq lsp-restart 'ignore)
      (setq lsp-modeline-code-actions-enable nil)
      (setq lsp-apply-edits-after-file-operations nil)
      (setq lsp-file-watch-threshold 5000)
      (add-hook 'prog-mode-hook #'lsp)
      :hook ((typescript-ts-mode . lsp)
             (python-mode . lsp)
             (yaml-mode . lsp)
             (json-mode . lsp)
             (css-mode . lsp)
             (bash-mode . lsp)
             (sh-mode . lsp))
      :commands lsp)
    
    (use-package lsp-ui
      :custom
      (lsp-ui-sideline-enable t)
      (lsp-ui-doc-enable t)
      (lsp-ui-doc--sideline-pos-y 0)
      (lsp-ui-doc-delay 0.5))
    
    (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
    (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
    
    (use-package lsp-pyright
      :ensure t
      :hook (python-mode . (lambda ()
                             (require 'lsp-pyright)
                             (lsp))))
    
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
    
    (use-package typescript-ts-mode
      :ensure t)
    
    ;; Project management
    (use-package projectile
      :ensure t
      :bind-keymap
      ("C-c p" . projectile-command-map)
      :config
      (projectile-mode +1)
      (setq projectile-project-search-path '("~/code")))
    
    ;; Navigation and editing tools
    (use-package ws-butler
      :ensure t
      :config
      (ws-butler-global-mode))
    
    (use-package switch-window
      :bind (("M-o" . switch-window))
      :custom
      (switch-window-shortcut-style 'qwerty))
    
    (use-package avy
      :bind
      ("C-/" . 'avy-goto-char-2)
      ("M-j" . 'avy-goto-char-timer))
    
    (use-package python
      :mode ("\\.py\\'" . python-ts-mode)
      :interpreter ("python" . python-ts-mode))
    
    (use-package markdown-mode
      :ensure t
      :mode ("\\.md\\'" . gfm-mode)
      :bind
      (:map markdown-mode-map
            ("<tab>" . markdown-cycle)
            ("S-<tab>" . markdown-shifttab))
      :init (setq markdown-hide-markup-in-view-modes t))
    
    ;; Git integration
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
      :config
      (global-diff-hl-mode)
      (diff-hl-flydiff-mode)
      (diff-hl-margin-mode))
    
    ;; Code completion
    (use-package company
      :init (global-company-mode)
      :config
      (setq company-idle-delay 0.5)
      :bind (:map company-active-map ("<enter>" . company-complete-selection)))
    
    (use-package copilot
      :vc (:url "https://github.com/copilot-emacs/copilot.el"
                :rev :newest
                :branch "main")
      :hook (prog-mode . copilot-mode)
;;       :bind (("<tab>" . copilot-accept-completion)
;;              ("C-TAB" . copilot-accept-completion)))
       :bind (:map copilot-completion-map
                   ("<tab>" . 'copilot-accept-completion)
                   ("TAB" . 'copilot-accept-completion)
                   ("C-TAB" . 'copilot-accept-completion-by-word)
                   ("C-<tab>" . 'copilot-accept-completion-by-word)
                   ("C-n" . 'copilot-next-completion)
                   ("C-p" . 'copilot-previous-completion)))

    ;; Search and file navigation
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
      :ensure t)
    
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
    
    (use-package counsel-projectile
      :config
      (counsel-projectile-mode)
      :bind
      (("C-c k" . 'counsel-projectile-rg)
       ("M-p" . 'counsel-projectile-find-file)
       ("C-c 4 f" . 'projectile-find-file-other-window)
       ("C-c C-f" . 'counsel-projectile-find-file)))
    
    (use-package ivy
      :bind
      (("C-o" . 'swiper))
      :custom
      (ivy-use-virtual-buffers t)
      (ivy-initial-inputs-alist nil)
      :config
      (ivy-mode nil))
    
    ;; Org mode (if needed)
    (use-package org
      :ensure t
      :demand t
      :bind (("C-c a" . org-agenda)
             ("C-c c" . org-capture)
             ("C-' o" . open-the-list)
             :map org-mode-map
             (("M-F" . org-metaright)
              ("M-B" . org-metaleft)
              ("C-c i t" . counsel-org-tag)
              ("<tab>" . org-cycle)
              ("S-<tab>" . org-shifttab)
              ("C-<tab>" . org-global-cycle)
              ("M-P" . org-metaup)
              ("M-N" . org-metadown)
              ("C-c o" . org-table-insert-row)
              ("C-c t i" . org-table-insert-row)
              ("C-c t p" . org-table-move-row-up)
              ("C-c t n" . org-table-move-row-down)
              ("C-c X" . org-latex-export-to-pdf)))
      :config
      (setq org-directory (file-truename "~/Dropbox/Org"))
      (setq the-list-file (concat org-directory "/the-list.org"))
      (defun open-the-list ()
        "Quickly edit my ~/Org/the-list.org file."
        (interactive)
        (find-file the-list-file))
      
      (setq org-latex-pdf-process '("pdflatex -output-directory=pdfs %f"))
      (setq org-time-stamp-formats '("%Y-%m-%d %a" . "%Y-%m-%d %a %I:%M%p"))
      (setq org-archive-location "archive/%s_archive::")
      (setq org-agenda-files (list org-directory))
      (setq org-agenda-remove-tags nil)
      (setq org-startup-indented nil)
      (setq org-hide-leading-stars nil)
      (setq org-startup-folded 'nofold)
      (setq org-blank-before-new-entry '((heading . nil)
                                          (plain-list-item . nil)))
      (setq org-capture-templates
            '(("t" "TODO" entry (file+headline tasks-file "Tasks")
               "* TODO %?\n  %i\n  %a")
              ("s" "Shopping" entry (file+headline tasks-file "Tasks")
               "* TODO %?%(org-set-tags \"BUY\")\n")))
      (require 'org-agenda))
    
    ;; Other tools
    (use-package expand-region
      :bind ("C-=" . er/expand-region))
    
    (use-package saveplace
      :init (save-place-mode 1)
      :config
      (progn
        (setq-default save-place t)
        (setq save-place-limit nil)))
    
    (use-package kbd-mode
      :vc (:url "https://github.com/kmonad/kbd-mode" :rev :newest))
    
    (use-package treemacs)
    (use-package treemacs-projectile)
    
    ;; Set flag to indicate full config is loaded
    (setq ted/full-config-loaded t)
    (message "Full configuration loaded successfully.")))

;; Bind key to load full configuration
(global-set-key (kbd "C-c L") 'ted/load-full-config)

;; Load full config immediately for GUI mode
(unless ted/is-terminal
  (ted/load-full-config))

;; If you want to load certain packages even in terminal mode, add them here

;; Minimal terminal packages
(when ted/is-terminal
  ;; Load a small set of essential packages for terminal mode
  (use-package magit
    :ensure t
    :bind (("C-c m" . magit-status)))

  (use-package ayu-theme
    :config (load-theme 'ayu-dark t))
  
  ;; Maybe add a lightweight completion framework
  (use-package counsel
    :config
    (ivy-mode 1)
    :bind
    (("M-x" . counsel-M-x)
     ("C-x C-f" . counsel-find-file)
     ("C-x b" . ivy-switch-buffer))))

;; Display a message in terminal mode
(when ted/is-terminal
  (message "Press C-c L to load full config."))

(provide 'init)
;;; init.el ends here
