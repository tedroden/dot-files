;; 
;; stolen from https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init-helm-thierry.el

;; (use-package helm-config
;;     :config
;;   (progn
;;     (helm-mode 1)
;;     (helm-adaptive-mode 1)
;;     (helm-push-mark-mode 1)
;;     (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)    
;;     (global-set-key (kbd "C-x b") 'helm-mini)    
;;     (global-set-key (kbd "C-x C-f") 'helm-find-files)
;;     (global-set-key (kbd "M-x")                          'undefined)
;;     (global-set-key (kbd "M-x")                          'helm-M-x)
;;     (global-set-key (kbd "C-h a")                        'helm-apropos)
;;     (global-set-key (kbd "M-y")                          'helm-show-kill-ring) ;; eh...
;;     (global-set-key (kbd "C-s")                          'helm-occur)    
;;     (setq helm-ff-file-name-history-use-recentf t)
;;     ))

;; (setq helm-ff-file-name-history-use-recentf t)
;; ;;;; Test Sources or new helm code. 
;; ;;   !!!WARNING EXPERIMENTAL!!!

;; (defun helm/version ()
;;   (with-temp-buffer
;;     (insert-file-contents (find-library-name "helm-pkg"))
;;     (goto-char (point-min))
;;     (when (re-search-forward
;;            "\\([0-9]+?\\)\\.\\([0-9]+?\\)\\.\\([0-9]+?\\)\\.?[0-9]*" nil t)
;;       (match-string-no-properties 0))))

;; (defun helm/git-version ()
;;   (shell-command-to-string
;;    "git log --pretty='format:%H' -1"))

;; (defun helm/turn-on-header-line ()
;;   (interactive)
;;   (setq helm-echo-input-in-header-line t)
;;   (setq helm-split-window-in-side-p t)
;;   (helm-autoresize-mode -1)
;;   (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
;;   )

;; (defun helm/turn-off-header-line ()
;;   (interactive)
;;   (setq helm-echo-input-in-header-line nil)
;;   ;;(helm-autoresize-mode 1)
;;   (setq helm-split-window-in-side-p nil)
;;   (remove-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
;;   )

;; (defun helm/occur-which-func ()
;;   (interactive)
;;   (with-current-buffer
;;       (or (helm-aif (with-helm-buffer
;;                       (window-buffer helm-persistent-action-display-window))
;;               (and (null (minibufferp it)) it))
;;           helm-current-buffer)
;;     (when (eq major-mode 'emacs-lisp-mode)
;;       (message "[%s]" (which-function)))))

;; (define-key helm-moccur-map (kbd "C-c ?") 'helm/occur-which-func)
;; (define-key helm-grep-map (kbd "C-c ?") 'helm/occur-which-func)

;; ;; Popup buffer-name or filename in grep/moccur/imenu-all etc...

;; (helm-popup-tip-mode 1)

;; ;; Show the visibles buffers on top of list (issue #1301)

;; (defun helm/modify-ido-temp-list ()
;;   (let ((bl (mapcar #'buffer-name (buffer-list (selected-frame)))))
;;     (setq ido-temp-list (nconc (cdr bl) (list (car bl))))))
;; ;;(add-hook 'ido-make-buffer-list-hook 'helm/modify-ido-temp-list)

;; 
;; ;;; Helm-command-map
;; ;;
;; ;;
;; (define-key helm-command-map (kbd "g")   'helm-apt)
;; (define-key helm-command-map (kbd "w")   'helm-psession)
;; (define-key helm-command-map (kbd "z")   'helm-complex-command-history)
;; (define-key helm-command-map (kbd "w")   'helm-w3m-bookmarks)
;; (define-key helm-command-map (kbd "x")   'helm-firefox-bookmarks)
;; (define-key helm-command-map (kbd "#")   'helm-emms)
;; (define-key helm-command-map (kbd "I")   'helm-imenu-in-all-buffers)

;; ;;; Global-map
;; ;;
;; ;;
;; (global-set-key (kbd "C-x b") 'helm-mini)
;; (global-set-key (kbd "M-x")                          'undefined)
;; (global-set-key (kbd "M-x")                          'helm-M-x)
;; (global-set-key (kbd "M-y")                          'helm-show-kill-ring)
;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; re
;; (global-set-key (kbd "C-x C-f")                      'helm-find-files)
;; (global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
;; (global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
;; ; (global-set-key (kbd "C-h r")                        'helm-info-emacs)
;; (global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
;; (global-set-key (kbd "C-,")                          'helm-calcul-expression)
;; ; (global-set-key (kbd "C-h i")                        'helm-info-at-point)
;; (global-set-key (kbd "C-x C-d")                      'helm-browse-project)
;; (global-set-key (kbd "<f1>")                         'helm-resume)
;; (global-set-key (kbd "C-h C-f")                      'helm-apropos)
;; (global-set-key (kbd "C-h a")                        'helm-apropos)
;; (global-set-key (kbd "<f5> s")                       'helm-find)
;; (global-set-key (kbd "<f2>")                         'helm-execute-kmacro)
;; (global-set-key (kbd "C-c i")                        'helm-imenu-in-all-buffers)

;; ;; This one creates a new buffer and shortens the window
;; ;; (global-set-key (kbd "C-s")                          'helm-occur)

;; (define-key global-map [remap jump-to-register]      'helm-register)
;; (define-key global-map [remap list-buffers]          'helm-mini)
;; (define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
;; (define-key global-map [remap find-tag]              'helm-etags-select)
;; (define-key global-map [remap xref-find-definitions] 'helm-etags-select)

;; ;; these ones error
;; ; (global-set-key (kbd "<f11> o")                      'helm-org-agenda-files-headings)
;; ;(define-key global-map (kbd "M-g a")                 'helm-do-grep-ag)
;; ;(define-key global-map (kbd "M-g g")                 'helm-grep-do-git-grep)
;; ;(define-key global-map (kbd "M-g i")                 'helm-gid)

;; ;; Shell bindings
;; (define-key shell-mode-map (kbd "M-p")               'helm-comint-input-ring) ; shell history.

;; (helm-multi-key-defun helm-multi-lisp-complete-at-point
;;     "Multi key function for completion in emacs lisp buffers.
;; First call indent, second complete symbol, third complete fname."
;;   '(helm-lisp-indent
;;     helm-lisp-completion-at-point
;;     helm-complete-file-name-at-point)
;;   0.3)

;; (if (and (boundp 'tab-always-indent)
;;          (eq tab-always-indent 'complete)
;;          (boundp 'completion-in-region-function))
;;     (progn
;;       (define-key lisp-interaction-mode-map [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)
;;       (define-key emacs-lisp-mode-map [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)
      
;;       ;; lisp complete. (Rebind M-<tab>)
;;       (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
;;       (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
    
;;     (define-key lisp-interaction-mode-map [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)
;;     (define-key emacs-lisp-mode-map [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)
    
;;     ;; lisp complete. (Rebind M-<tab>)
;;     (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
;;     (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;; (unless (boundp 'completion-in-region-function)
;;   (add-hook 'ielm-mode-hook
;;             #'(lambda ()
;;                 (define-key ielm-map    [remap completion-at-point] 'helm-lisp-completion-at-point))))

;; ;;; helm find files
;; ;;
;; (define-key helm-find-files-map (kbd "C-d") 'helm-ff-persistent-delete)
;; (define-key helm-buffer-map (kbd "C-d")     'helm-buffer-run-kill-persistent)

;; ;; Use default-as-input in grep
;; (add-to-list 'helm-sources-using-default-as-input 'helm-source-grep)
;; (add-to-list 'helm-sources-using-default-as-input 'helm-source-grep-ag)

;; ;;; Describe key-bindings
;; ;;
;; ;;
;; ; (helm-descbinds-install)            ; C-h b, C-x C-h
