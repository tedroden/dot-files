;;; init.el --- Emacs configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Requires Emacs 30+ (due to: `use-package :vc`)
;;
;;; I'm currently installing this emacs:
;; brew tap d12frosted/emacs-plus
;; brew install emacs-plus@31
;;
;; osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@30/Emacs.app" at POSIX file "/Applications" with properties {name:"Emacs.app"}'
;;
;; DO NOT reinstall, uninstall and install again.
;; Do this: `brew uninstall emacs-plus@30 && brew unlink emacs-plus@30 && rm /Applications/Emacs.app` and reinstall it.

;;; Code:

;; Core package setup
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

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Suppress warnings
(setq byte-compile-warnings '(not obsolete cl-functions interactive-only))
(setq warning-minimum-level :error)
(setq warning-minimum-log-level :warning)

;; Setup PATH from shell on Mac
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

;; Add lisp/ to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load modules
(require 'ted-core)
(require 'ted-ui)
(require 'ted-tools)
(require 'ted-programming)

;; Load custom and personal files
(setq-default dotfiles-dir (file-truename "~/.emacs.d/")
              custom-file (concat dotfiles-dir "custom.el")
              personal-file (concat dotfiles-dir "personal.el"))

(dolist (f (list custom-file personal-file))
  (when (file-exists-p f)
    (load f)))

;; Bootstrap straight.el
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

;; Config file quick-open commands
(require 'config-file-commands)

(defvar ted/config-files
  '(("t" "tmux" "~/code/dot-files/tmux/.tmux.conf")
    ("z" "zsh" "~/code/dot-files/zsh/.zshrc")
    ("e" "emacs" "~/code/dot-files/emacs/dot-emacs.d/init.el"))
  "List of config files with (key description path) format.")

(create-config-commands ted/config-files "C-c c")

(provide 'init)
;;; init.el ends here
