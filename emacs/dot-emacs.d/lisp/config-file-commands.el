;;; config-file-commands.el --- Quick access commands for config files -*- lexical-binding: t -*-

;;; Commentary:

;; This module provides a convenient way to create quick-access commands
;; for editing configuration files.
;;
;; Usage:
;;   1. Define your config files list in your init.el:
;;      (defvar my/config-files
;;        '(("t" "tmux" "~/.tmux.conf")
;;          ("z" "zsh" "~/.zshrc")
;;          ("e" "emacs" "~/.emacs.d/init.el")))
;;
;;   2. Require this module:
;;      (require 'config-file-commands)
;;
;;   3. Create the commands:
;;      (create-config-commands my/config-files "C-c c")
;;
;; This will:
;;   - Create a prefix keymap at "C-c c"
;;   - Create individual commands for each config file
;;   - Bind each command to its specified key under the prefix
;;
;; For example, with the above config, pressing "C-c c t" will open
;; your tmux config file.
;;
;; Each config entry should be a list of three elements:
;;   (KEY DESCRIPTION PATH)
;; where:
;;   KEY         - The key to bind (e.g., "t", "z", "e")
;;   DESCRIPTION - Short name for the config (e.g., "tmux", "zsh")
;;   PATH        - Full path to the config file

;;; Code:

(defun edit-config-file (path)
  "Edit config file at PATH.
Expands the path before opening the file."
  (find-file (expand-file-name path)))

(defun create-config-commands (config-files keymap-prefix)
  "Create interactive commands and keybindings for CONFIG-FILES.

CONFIG-FILES should be a list of (KEY DESCRIPTION PATH) entries.
KEYMAP-PREFIX is the key sequence for the prefix map (e.g., \"C-c c\").

For each entry in CONFIG-FILES:
- Creates a command named 'edit-DESCRIPTION-config'
- Binds it to KEY under the prefix keymap

Example:
  (create-config-commands
    '((\"t\" \"tmux\" \"~/.tmux.conf\")
      (\"z\" \"zsh\" \"~/.zshrc\"))
    \"C-c c\")"
  (let ((config-map (make-sparse-keymap)))
    (global-set-key (kbd keymap-prefix) config-map)
    (dolist (config config-files)
      (let* ((key (nth 0 config))
             (name (nth 1 config))
             (path (nth 2 config))
             (func-name (intern (concat "edit-" name "-config"))))
        ;; Create the command
        (defalias func-name
          `(lambda ()
             ,(format "Edit %s configuration file." name)
             (interactive)
             (edit-config-file ,path)))
        ;; Bind it to the key in our config map
        (define-key config-map (kbd key) func-name)))))

(provide 'config-file-commands)
;;; config-file-commands.el ends here
