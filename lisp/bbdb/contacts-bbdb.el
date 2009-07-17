;;; contacts-bbdb.el --- Syncs between Apple's Addressbook and BBDB

;; Copyright (C) 2004  Jonas Steverud <tvrud@bredband.net>

;; Author: Jonas Steverud <tvrud@bredband.net>
;; Version: 1.0
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Anyone that uses BBDB and the Address Book that is built-in Mac OS X
;; finds sooner or later that they becomes out of sync and most
;; important are hard to update automatically.

;; The contacts program developed by "shane (at) gnufoo (dot) org"
;; makes it possible to dump the output of the Address Book unto
;; standard out. This file parses that output and compares it to
;; BBDB. If they differ the BBDB is updates. The contacts program
;; does not support modification of Address Book and hence this
;; program doesn't either.

;;; Code:

(require 'bbdb)
;;(bbdb-initialize 'gnus 'message)

(defvar contacts-bbdb-program "contacts"
  "The name of the contacts program.")

(defvar contacts-bbdb-parameter-format "%fn\t%ln\t%c\t%nn\t%he\t%we\t%oe\t%u"
  "The arguments sent to the program. If changes from the default,
the corresponding parsing function\(s\) must change as well.")

(defvar contacts-bbdb-buffer "*Contacts*"
  "Working temporary buffer for interaction with the contacts
  program.")

;; (defvar contacts-bbdb-id-tag 'address-book-unique-id
;;   "The unique ID tag from the Address Book is stored in the BBDB
;; under custom fiield named according to this variable's
;; value. Default is address-book-unique-id.")

(defvar contacts-bbdb-excluded-regexps nil
  "List of regexps, if any of them matches, that individual will
be excluded. Each element in the list can be either of the following:

* A string; a regexp that will be applied to the full
  name (firstname lastname) and all email addresses.

* A pair (field . regexp) where field is any of the symbols
   + name (full name in the form \"firstname lastname\")
   + email (all email addresses one by one)
   + company

* A symbol which will be funcall'ed with the parsed data from
  Contacts. The individual will be excluded if the function
  returns non-nil. The arguments are the fields (in that order):
  Firstname Lastname Company Nickname Homeemail Workemail
  Otheremail ID")

(defun contacts-bbdb-sync ()
  "Main function for synchronizing between the Address Book and BBDB."
  (interactive)
  (let (BUF
	(case-fold-search t))
    (setq BUF (generate-new-buffer contacts-bbdb-buffer))
    (switch-to-buffer BUF)
    (let ((coding-system-for-read 'mac-roman))
      (let ((params (append (delete nil (list
					 "-H"
					 "-l"
					 ))
			    '("-f")
			    (list contacts-bbdb-parameter-format)
			    )))

	(apply 'call-process contacts-bbdb-program nil '(t t) nil
	       ;; Program parameters:
	       params)
	) ;; let

      ;; Convert from mac-roman to the default coding system - which
      ;; probably is the same as the one BBDB is endoded in.
      (encode-coding-region (point-min) (point-max)
			    (default-value 'buffer-file-coding-system))
      )	;; let
    
    (beginning-of-buffer)
    (let (min max str pers rec
	      FNAME LNAME COMPANY NICK HOME WORK OTHEREMAIL ID)
      (while (not (eobp))
	(setq pers (split-string
		    (buffer-substring (point)
				      (progn (end-of-line) (point))) " *\t" nil)
	      FNAME (nth 0 pers)
	      LNAME (nth 1 pers)
	      COMPANY (nth 2 pers)
	      NICK (nth 3 pers)
	      HOME (nth 4 pers)
	      WORK (nth 5 pers)
	      OTHEREMAIL (nth 6 pers)
	      ID (nth 7 pers)
	      )
;; 	(if (> (length FNAME) 0)
;; 	    ;; Clean out the leading space.
;; 	    (setq FNAME (substring FNAME 1)))

	;;	(message "FNAME=%s, LNAME=%s, COMPANY=%s, NICK=%s, HOME=%s, WORK=%s, OTHEREMAIL=%s, ID=%s." FNAME LNAME COMPANY NICK HOME WORK OTHEREMAIL ID)

	;; Anyone to be excluded?
	(if (not (contacts-bbdb-exclude-p FNAME LNAME COMPANY NICK HOME WORK OTHEREMAIL ID))
	
	    (if (delete "" (list HOME WORK OTHEREMAIL))
		;; If any address is != "", the result becomes non-nil
		(progn
		  ;; Skip anyone that does not have an email address.
		  (setq rec (or (bbdb-search-simple nil HOME)
				(bbdb-search-simple nil WORK)
				(bbdb-search-simple nil OTHEREMAIL)))
		  (if rec
		      ;; The individual in Contacts exists in BBDB
		      (progn
			;; Email shall be updated no matter what
			(let ((lst (bbdb-record-net rec)))
			  (add-to-list 'lst HOME)
			  (add-to-list 'lst WORK)
			  (add-to-list 'lst OTHEREMAIL)
			  (bbdb-record-set-net rec (delete "" lst))
			  )		; let
			;; We really don't care if the individual has been
			;; synced before or not, update all fields. The
			;; unique id tag is not used for anything - yet.
			(let ((lst (bbdb-record-aka rec)))
			  (add-to-list 'lst NICK)
			  (bbdb-record-set-aka rec (delete "" lst))
			  ) ;; let
			;; Set the name and company
			(bbdb-record-set-firstname rec FNAME)
			(bbdb-record-set-lastname  rec LNAME)
			(bbdb-record-set-company   rec COMPANY)
		      
			(let ((lst (bbdb-record-getprop rec 'mail-alias)))
			  (if (and lst
				   NICK
				   (not (string-equal "" NICK)))
			      (progn
				(setq lst (split-string lst ","))
				(add-to-list 'lst NICK)
				(bbdb-record-putprop rec 'mail-alias
						     (mapconcat 'identity lst ",")
						     )
				))) ;; let
		      
			)
		    ;; else
		    ;; The individual in Contatacs does *not* exist in BBDB
		    (progn
		      (setq rec (vector FNAME ;; Förnamn
					LNAME ;; Efternamn
					nil   ;; Alias/AKA
					COMPANY	;; company
					nil	;; phones
					nil	;; addrs
					(delete "" (list HOME WORK OTHEREMAIL))	;; net
					nil ;; notes
					(make-vector bbdb-cache-length nil)))
		      ;; Lägg till ID:t
		      ;;(bbdb-record-putprop rec contacts-bbdb-id-tag ID)
		      (bbdb-create rec)
		      )
		    )			; if rec and string-equal
	    
		  )    
	      )	;; if (delete "" (list HOME WORK OTHEREMAIL))
	  (message "%s %s (%s) excluded." FNAME LNAME COMPANY)
	  )	;; if excluded
	(next-line)
	(beginning-of-line)
	) ;; while
      
      )	;; let
    
    ) ;; let
  
  ;; xxx
  ) ;; contacts-bbdb-sync


(defun contacts-bbdb-exclude-p (FNAME LNAME COMPANY NICK HOME WORK OTHEREMAIL ID)
  "Returns non-nil if the individual shall be excluded."

  (delete nil
	  (mapcar (function (lambda (E)
			      (cond
			       ((functionp E)
				(funcall E FNAME LNAME COMPANY NICK HOME WORK OTHEREMAIL ID))
			       ((stringp E)
				(or (string-match E (concat FNAME " " LNAME))
				    (string-match E HOME)
				    (string-match E WORK)
				    (string-match E OTHEREMAIL)))
			       ((consp E)
				(let ((sym (car E))
				      (regexp (cdr E)))
				  (cond
				   ((equal sym 'company) (string-match regexp COMPANY))
				   ((equal sym 'name) (string-match regexp (concat FNAME " " LNAME)))
				   ((equal sym 'email) (or (string-match regexp HOME)
							   (string-match regexp WORK)
							   (string-match regexp OTHEREMAIL)))
				   ) ;; cond
				  ) ;; let
				) ;; consp E

			       (t (error "Error parsing contacts-bbdb-excluded-regexps, %s is not a valid value." (prin1-to-string '(lambda (E) (list E)))))
			       ) ;; cond
			      
			      ))
		  contacts-bbdb-excluded-regexps)
	  ) ;; delete
  )

(provide 'contacts-bbdb)
;;; contacts-bbdb.el ends here
