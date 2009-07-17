;;; muse-autoloads.el --- autoloads for Muse
;;
;;; Code:

;;;### (autoloads (muse-colors-toggle-inline-images) "muse-colors"
;;;;;;  "muse-colors.el" (17734 779))
;;; Generated autoloads from muse-colors.el

(autoload (quote muse-colors-toggle-inline-images) "muse-colors" "\
Toggle inlined images on/off.

\(fn)" t nil)

;;;***

;;;### (autoloads (muse-import-docbook-files muse-import-docbook)
;;;;;;  "muse-import-docbook" "muse-import-docbook.el" (17714 64097))
;;; Generated autoloads from muse-import-docbook.el

(autoload (quote muse-import-docbook) "muse-import-docbook" "\
Convert the Docbook buffer SRC to Muse, writing output in the DEST buffer.

\(fn SRC DEST)" t nil)

(autoload (quote muse-import-docbook-files) "muse-import-docbook" "\
Convert the Docbook file SRC to Muse, writing output to the DEST file.

\(fn SRC DEST)" t nil)

;;;***

;;;### (autoloads (muse-import-latex) "muse-import-latex" "muse-import-latex.el"
;;;;;;  (17620 46883))
;;; Generated autoloads from muse-import-latex.el

(autoload (quote muse-import-latex) "muse-import-latex" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (muse-message-markup) "muse-message" "../experimental/muse-message.el"
;;;;;;  (17429 57860))
;;; Generated autoloads from ../experimental/muse-message.el

(autoload (quote muse-message-markup) "muse-message" "\
Markup a wiki-ish e-mail message as HTML alternative e-mail.
This step is manual by default, to give the author a chance to review
the results and ensure they are appropriate.
If you wish it to be automatic (a risky proposition), just add this
function to `message-send-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads (muse-insert-tag muse-index muse-find-backlinks
;;;;;;  muse-search muse-search-with-command muse-what-changed muse-previous-reference
;;;;;;  muse-next-reference muse-follow-name-at-point-other-window
;;;;;;  muse-follow-name-at-point muse-browse-result muse-edit-link-at-point
;;;;;;  muse-insert-relative-link-to-file muse-decrease-list-item-indentation
;;;;;;  muse-increase-list-item-indentation muse-insert-list-item
;;;;;;  muse-mode-choose-mode muse-mode) "muse-mode" "muse-mode.el"
;;;;;;  (17809 46981))
;;; Generated autoloads from muse-mode.el

(autoload (quote muse-mode) "muse-mode" "\
Muse is an Emacs mode for authoring and publishing documents.
\\{muse-mode-map}

\(fn)" t nil)

(autoload (quote muse-mode-choose-mode) "muse-mode" "\
Turn the proper Emacs Muse related mode on for this file.

\(fn)" nil nil)

(autoload (quote muse-insert-list-item) "muse-mode" "\
Insert a list item at the current point, taking into account
your current list type and indentation level.

\(fn)" t nil)

(autoload (quote muse-increase-list-item-indentation) "muse-mode" "\
Increase the indentation of the current list item.

\(fn)" t nil)

(autoload (quote muse-decrease-list-item-indentation) "muse-mode" "\
Decrease the indentation of the current list item.

\(fn)" t nil)

(autoload (quote muse-insert-relative-link-to-file) "muse-mode" "\
Insert a relative link to a file, with optional description, at point.

\(fn)" t nil)

(autoload (quote muse-edit-link-at-point) "muse-mode" "\
Edit the current link.
Do not rename the page originally referred to.

\(fn)" t nil)

(autoload (quote muse-browse-result) "muse-mode" "\
Visit the current page's published result.

\(fn STYLE &optional OTHER-WINDOW)" t nil)

(autoload (quote muse-follow-name-at-point) "muse-mode" "\
Visit the link at point.

\(fn &optional OTHER-WINDOW)" t nil)

(autoload (quote muse-follow-name-at-point-other-window) "muse-mode" "\
Visit the link at point in other window.

\(fn)" t nil)

(autoload (quote muse-next-reference) "muse-mode" "\
Move forward to next Muse link or URL, cycling if necessary.

\(fn)" t nil)

(autoload (quote muse-previous-reference) "muse-mode" "\
Move backward to the next Muse link or URL, cycling if necessary.
In case of Emacs x <= 21 and ignoring of intangible properties (see
`muse-mode-intangible-links').

This function is not entirely accurate, but it's close enough.

\(fn)" t nil)

(autoload (quote muse-what-changed) "muse-mode" "\
Show the unsaved changes that have been made to the current file.

\(fn)" t nil)

(autoload (quote muse-search-with-command) "muse-mode" "\
Search for the given TEXT string in the project directories
using the specified command.

\(fn TEXT)" t nil)

(autoload (quote muse-search) "muse-mode" "\
Search for the given TEXT using the default grep command.

\(fn)" t nil)

(autoload (quote muse-find-backlinks) "muse-mode" "\
Grep for the current pagename in all the project directories.

\(fn)" t nil)

(autoload (quote muse-index) "muse-mode" "\
Display an index of all known Muse pages.

\(fn)" t nil)

(autoload (quote muse-insert-tag) "muse-mode" "\
Insert a tag interactively with a blank line after it.

\(fn TAG)" t nil)

;;;***

;;;### (autoloads (muse-project-publish muse-project-publish-this-file
;;;;;;  muse-project-find-file) "muse-project" "muse-project.el"
;;;;;;  (18008 65516))
;;; Generated autoloads from muse-project.el

(autoload (quote muse-project-find-file) "muse-project" "\
Open the Muse page given by NAME in PROJECT.
If COMMAND is non-nil, it is the function used to visit the file.
If DIRECTORY is non-nil, it is the directory in which the page
will be created if it does not already exist.  Otherwise, the
first directory within the project's fileset is used.

\(fn NAME PROJECT &optional COMMAND DIRECTORY)" t nil)

(autoload (quote muse-project-publish-this-file) "muse-project" "\
Publish the currently-visited file according to `muse-project-alist',
prompting if more than one style applies.

If FORCE is given, publish the file even if it is up-to-date.

\(fn &optional FORCE)" t nil)

(autoload (quote muse-project-publish) "muse-project" "\
Publish the pages of PROJECT that need publishing.

\(fn PROJECT &optional FORCE)" t nil)

;;;***

;;;### (autoloads (muse-browse-url) "muse-protocols" "muse-protocols.el"
;;;;;;  (17991 51637))
;;; Generated autoloads from muse-protocols.el

(autoload (quote muse-browse-url) "muse-protocols" "\
Handle URL with the function specified in `muse-url-protocols'.
If OTHER-WINDOW is non-nil, open in a different window.

\(fn URL &optional OTHER-WINDOW)" t nil)

;;;***

;;;### (autoloads (muse-publish-this-file muse-publish-file muse-publish-region)
;;;;;;  "muse-publish" "muse-publish.el" (18013 46244))
;;; Generated autoloads from muse-publish.el

(autoload (quote muse-publish-region) "muse-publish" "\
Apply the given STYLE's markup rules to the given region.
The result is placed in a new buffer that includes TITLE in its name.

\(fn BEG END &optional TITLE STYLE)" t nil)

(autoload (quote muse-publish-file) "muse-publish" "\
Publish the given FILE in a particular STYLE to OUTPUT-DIR.
If the argument FORCE is nil, each file is only published if it is
newer than the published version.  If the argument FORCE is non-nil,
the file is published no matter what.

\(fn FILE STYLE &optional OUTPUT-DIR FORCE)" t nil)

(autoload (quote muse-publish-this-file) "muse-publish" "\
Publish the currently-visited file.
Prompt for both the STYLE and OUTPUT-DIR if they are not
supplied.

\(fn STYLE OUTPUT-DIR &optional FORCE)" t nil)

;;;***

;;;### (autoloads nil nil ("../contrib/cgi.el" "../contrib/httpd.el"
;;;;;;  "../experimental/muse-cite.el" "../experimental/muse-mathml.el"
;;;;;;  "../experimental/muse-protocol-iw.el" "../experimental/muse-split.el"
;;;;;;  "muse-backlink.el" "muse-book.el" "muse-docbook.el" "muse-groff.el"
;;;;;;  "muse-html.el" "muse-http.el" "muse-import-xml.el" "muse-journal.el"
;;;;;;  "muse-latex.el" "muse-latex2png.el" "muse-poem.el" "muse-regexps.el"
;;;;;;  "muse-texinfo.el" "muse-wiki.el" "muse-xml-common.el" "muse-xml.el"
;;;;;;  "muse.el") (18016 57296 406650))

;;;***

;;;### (autoloads (muse-blosxom-new-entry) "muse-blosxom" "muse-blosxom.el"
;;;;;;  (17989 60302))
;;; Generated autoloads from muse-blosxom.el

(autoload (quote muse-blosxom-new-entry) "muse-blosxom" "\
Start a new blog entry with given CATEGORY.
The filename of the blog entry is derived from TITLE.
The page will be initialized with the current date and TITLE.

\(fn CATEGORY TITLE)" t nil)

;;;***

(provide 'muse-autoloads)
;;; muse-autoloads.el ends here
;;
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

