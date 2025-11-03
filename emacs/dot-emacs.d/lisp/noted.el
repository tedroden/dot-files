;;; noted.el --- Simple daily note taking system -*- lexical-binding: t -*-

;;; Commentary:
;; A simple markdown-based daily note taking system
;; Files are organized by year/month in ~/Dropbox/mem/daily/YYYY/MM/YYYY-MM-DD.md
;;
;; Dependencies:
;; - counsel (for counsel-rg in noted-search function)

;;; Code:

(defvar noted-root "~/Dropbox/noted"
  "Root directory for daily notes.")

(defun noted--format-date-full (time)
  "Format TIME as 'Friday, October 31st, 2025'."
  (let* ((day-name (format-time-string "%A" time))
         (month-name (format-time-string "%B" time))
         (day-num (string-to-number (format-time-string "%d" time)))
         (year (format-time-string "%Y" time))
         (suffix (cond
                  ((memq day-num '(11 12 13)) "th")
                  ((= (% day-num 10) 1) "st")
                  ((= (% day-num 10) 2) "nd")
                  ((= (% day-num 10) 3) "rd")
                  (t "th"))))
    (format "%s, %s %d%s, %s" day-name month-name day-num suffix year)))

(defun noted--format-time-12hr (time)
  "Format TIME as '3:24pm'."
  (downcase (format-time-string "%-I:%M%p" time)))

(defun noted--get-file-path (&optional time)
  "Get the file path for the daily note at TIME (defaults to now)."
  (let* ((time (or time (current-time)))
         (year (format-time-string "%Y" time))
         (month (format-time-string "%m" time))
         (date (format-time-string "%Y-%m-%d" time))
         (dir (expand-file-name (concat year "/" month) noted-root))
         (file (expand-file-name (concat date ".md") dir)))
    (list dir file)))

(defun noted--create-initial-content (time)
  "Create the initial content for a daily note at TIME."
  (let ((date-str (noted--format-date-full time))
        (time-str (noted--format-time-12hr time)))
    (concat "---\n"
            "Daily: " date-str "\n"
            "Created: " time-str "\n"
            "---\n\n"
            "# ")))

(defun noted-goto-today ()
  "Open today's daily note."
  (interactive)
  (let* ((time (current-time))
         (path-info (noted--get-file-path time))
         (dir (car path-info))
         (file (cadr path-info))
         (buffer (get-file-buffer file)))

    ;; Ensure directory exists
    (unless (file-exists-p dir)
      (make-directory dir t))

    ;; If buffer already exists, just switch to it
    (if buffer
        (switch-to-buffer buffer)
      ;; Otherwise, open the file
      (let ((file-exists (file-exists-p file)))
        (find-file file)
        ;; If new file, insert template and position cursor
        (unless file-exists
          (insert (noted--create-initial-content time))
          ;; Cursor is already at the end after insert
          )))))

(defun noted-search ()
  "Search through noted files using ripgrep."
  (interactive)
  (let ((default-directory (expand-file-name noted-root)))
    (call-interactively 'counsel-rg)))

;; Set up keybindings under C-c d prefix
(define-prefix-command 'noted-map)
(global-set-key (kbd "C-c n") 'noted-map)
(define-key noted-map (kbd "t") 'noted-goto-today)
(define-key noted-map (kbd "r") 'noted-search)

(provide 'noted)
;;; noted.el ends here
