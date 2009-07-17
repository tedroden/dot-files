
(add-to-list 'load-path "~/.emacs.d/lisp/org")
(add-to-list 'load-path "~/.emacs.d/lisp/muse")
(add-to-list 'load-path "~/.emacs.d/lisp/planner")
(add-to-list 'load-path "~/.emacs.d/lisp/remember")
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-wiki")

(require 'emacs-wiki)
(require 'muse-mode)
(require 'muse-project)
(require 'muse-journal)
(require 'muse-html)
(require 'muse-blosxom)
(require 'muse-wiki)
(require 'muse-colors)
(require 'muse-latex)
(require 'muse-texinfo)

(defun teds-wikiwords-on () 
  "Turn muse-wiki-allow-nonexistent-wikiword on"
  (interactive)
  (setq muse-wiki-allow-nonexistent-wikiword t)
  (insert "on")
)
(defun teds-wikiwords-off () 
  "Turn muse-wiki-allow-nonexistent-wikiword nil"
  (interactive)
  (setq muse-wiki-allow-nonexistent-wikiword nil)
 )

(setq muse-wiki-allow-nonexistent-wikiword t)
(setq planner-carry-tasks-forward 0)

;(define-key global-map [f2] 'planner-create-task-from-buffer)
(global-set-key [f1] 'plan)
(global-set-key [f2] 'planner-create-task)
(global-set-key (kbd "<C-M-f2>") 'planner-create-task-from-buffer)
(global-set-key (kbd "<M-f2>") 'remember)


; (add-hook 'planner-mode-hook 'muse-mode)

(require 'planner)
(require 'planner-multi)
(require 'planner-id)
;(require 'remember-planner)

(require 'planner-gnus)
(planner-gnus-insinuate)


(require 'planner-diary)
(setq planner-diary-use-diary t)
(planner-insinuate-diary)
(planner-insinuate-calendar)
(setq planner-diary-number-of-days 7)
(setq planner-diary-number-of-diary-entries 7)
(setq planner-diary-file diary-file)


(defun planner-open-message-url (url) 
  "Open a message:// url in Mail.app"
  (browse-url url)
  t)


(planner-add-protocol "message:/?/?" 'planner-open-message-url nil)




(global-set-key "\C-cpl" 'muse-blosxom-new-entry)

(setq remember-annotation-functions planner-annotation-functions)
(setq remember-handler-functions '(remember-planner-append))
(setq remember-append-to-planner-hook
      '(remember-planner-add-timestamp))

(setq remember-append-to-planner-hook
	  '(remember-planner-add-timestamp remember-planner-add-xref))


