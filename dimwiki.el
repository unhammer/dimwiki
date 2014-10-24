;;; dimwiki.el --- a dim-witted wiki parser and lookup tool

;; Copyright (C) 2014  Kevin Brubeck Unhammer <unhammer@fsfe.org>

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Keywords: help, hypermedia

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is very simple mediawiki "parser" which the author uses for
;; getting quick abstracts of an article (similar to what the search
;; engine DuckDuckGo shows) in the minibuffer.

;;; Code:

(require 'json)
(require 'url)
(require 'ert)

(defvar dimwiki-wikidomains '("nn.wikipedia.org"
			      "no.wikipedia.org"
			      "en.wikipedia.org")
  "Domains tried (in order) by `dimwiki-search'.")

(defvar dimwiki-redirect-keywords '("#ADKAS"
				    "#ALIDIREKTU"
				    "#OMDIRIGER"
				    "#OMDIRIGERING"
				    "#REDIRECCIÓN"
				    "#REDIRECT"
				    "#REDIRECTION"
				    "#WEITERLEITUNG"
				    "#YÖNLENDİR")
  "Keywords that signal we should follow a redirect when searching.
https://en.wikipedia.org/wiki/Help:Redirect has links to more languages")

(defvar dimwiki-max-tokens 200
  "Told you it was stupid")

;;; Tokeniser:

(defconst dimwiki-nontext
  '( ?\[
     ?\]
     ?{
     ?}
     ?'
     ?| ))
(defconst dimwiki-nontext-re
  (regexp-opt (mapcar 'char-to-string dimwiki-nontext)))

(defun dimwiki-eat (from)
  (let ((char (char-after from)))
    (if (memq char dimwiki-nontext)
	(dimwiki-eat-char char)
      (dimwiki-eat-text))))

(defun dimwiki-eat-text ()
  (when (re-search-forward dimwiki-nontext-re (point-max) 'noerror)
    (goto-char (- (match-end 0) 1))))

(defun dimwiki-eat-char (char)
  (while (and (eq (char-after (point)) char)
	      (< (point) (point-max)))
    (forward-char)))

(defun dimwiki-tokenise-buffer ()
  (let ((i 0)
	(from (point))
	tokens)
    (while (and (< (point) (point-max))
		(< i dimwiki-max-tokens))
      (setq i (+ 1 i))
      (dimwiki-eat from)
      (setq tokens (cons (buffer-substring-no-properties from (point)) tokens)
	    from (point)))
    (reverse tokens)))

(defun dimwiki-tokenise-string (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (dimwiki-tokenise-buffer)))


;;; Parser:
(defvar dimwiki-tokens '()
  "Modified imperatively, but with dynamic scope.")
(defun dimwiki-gettok ()
  (and dimwiki-tokens (pop dimwiki-tokens)))
(defun dimwiki-peektok ()
  (and dimwiki-tokens (car dimwiki-tokens)))

(defun dimwiki-parse (string)
  (let ((dimwiki-tokens (dimwiki-tokenise-string string)))
    (cons 'doc (dimwiki-doc))))

(defun dimwiki-doc ()
  (let (parsed cur-tok)
    (catch 'break
      (while (setq cur-tok (dimwiki-peektok))
	(cond ((equal "[[" cur-tok)
	       (dimwiki-gettok)
	       (setq parsed (cons (dimwiki-link) parsed)))
	      ;; TODO: since we simply return nil here, -tmpl has to
	      ;; handle ]] and -link handle }} to avoid stopping the parse
	      ;; process -- not good.
	      ((equal "]]" cur-tok)
	       (throw 'break nil))
	      ((equal "|" cur-tok)
	       (throw 'break nil))
	      ((equal "{{" cur-tok)
	       (dimwiki-gettok)
	       (setq parsed (cons (dimwiki-tmpl) parsed)))
	      ((equal "}}" cur-tok)
	       (throw 'break nil))
	      (cur-tok
	       (dimwiki-gettok)
	       (setq parsed (cons cur-tok parsed)))
	      (t (throw 'break nil)))))
    (reverse parsed)))


;;; TODO: external/single-bracket [http://foo links]

(defun dimwiki-link ()		; we've seen a [[
  (let ((parsed (dimwiki-doc))
	(cur-tok (dimwiki-gettok)))
    (cond ((equal "]]" cur-tok)
	   (cons 'link parsed))
	  ((equal "|" cur-tok)
	   (let* ((parsed-end (dimwiki-link))
		  (tag (car parsed-end))
		  (text (cdr parsed-end))
		  (ref (cons 'ref parsed)))
	     ;; E.g. '(link (ref "Cake") "cakes")
	     (cons tag (cons ref text))))
	  (t (cons 'link? parsed)))))

(defun dimwiki-tmpl ()		; we've seen a {{
  (let ((parsed (dimwiki-doc))
	(cur-tok (dimwiki-gettok)))
    (cond ((equal "}}" cur-tok)
	   (cons 'tmpl parsed))
	  ((equal "|" cur-tok)
	   (let* ((parsed-end (dimwiki-tmpl))
		  (tag (car parsed-end))
		  (text (cdr parsed-end))
		  (ref (cons 'ref parsed)))
	     (cons tag (cons ref text))))
	  (t (cons 'tmpl? parsed)))))

(ert-deftest dimwiki-parse ()
  :tags '(dimwiki)
  (should (equal (dimwiki-parse "foo {{fie blah}} bar")
		 '(doc "foo "
		       (tmpl "fie blah")
		       " bar")))
  (should (equal (dimwiki-parse "foo [[fie blah]] bar")
		 '(doc "foo "
		       (link "fie blah")
		       " bar")))
  (should (equal (dimwiki-parse "foo {{fie[[with ]]blah}} bar")
		 '(doc "foo "
		       (tmpl "fie" (link "with ") "blah")
		       " bar")))
  (should (equal (dimwiki-parse "foo '''meh''' bar")
		 '(doc "foo "
		       "'''"		; too stupid to parse these :-)
		       "meh"
		       "'''"
		       " bar")))
  (should (equal (dimwiki-parse "foo [[ {{fie [[fum foo]] blah}} ]] bar")
		 '(doc "foo "
		       (link " "
			     (tmpl "fie "
				   (link "fum foo")
				   " blah")
			     " ")
		       " bar")))
  (should (equal (dimwiki-parse "foo [[fie|blah]] bar")
		 '(doc "foo "
		       (link (ref "fie") "blah")
		       " bar"))))

(defun dimwiki-run-tests ()
  (ert-run-tests-interactively 'dimwiki-parse))






;;; Network, json stuff:

(defun dimwiki-delete-http-header ()
  "Delete the HTTP header in the current buffer."
  (goto-char (point-min))
  (let ((beg (point-min))
	(end (search-forward "\n\n" nil t)))
    (when end
      (delete-region beg end))))

(defun dimwiki-send-search-query (term domain)
  (let ((url-request-method "GET")
	(url-request-data "")
	(url-request-extra-headers
	 '(("Accept-Language" . "en")
	   ("Accept-Charset" . "utf-8")))
	(get-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
		    (list (cons "titles" term)
			  (cons "format" "json")
			  (cons "action" "query")
			  (cons "prop" "revisions")
			  (cons "rvprop" "content"))
		    "&")))
    (with-current-buffer (url-retrieve-synchronously
			  (format "https://%s/w/api.php?%s" domain get-data))
      (unwind-protect
	  (let ((results ""))
	    (dimwiki-delete-http-header)
	    (setq results (buffer-string))
	    results)
	(kill-buffer)))))

(defun dimwiki-extract-article-from-json (json-string)
  (let ((json-object-type 'plist)
	(json-array-type 'list))
    (plist-get
     (car
      (plist-get
       (cadr
	(plist-get
	 (plist-get
	  (json-read-from-string json-string)
	  :query)
	 :pages))
       :revisions))
     :*)))

(defun dimwiki-skiplink (elt)
  (or (and (stringp (car elt))
	   (string-match "^Kategori" (car elt)))
      (and (listp (car elt))
	   (stringp (car (cdar elt)))
	   (string-match "\\.jpg" (car (cdar elt))))))


;;; TODO: less recursive, fails on long texts (thus dimwiki-max-tokens)
(defun dimwiki-justext (parsed)
  (let ((head (car parsed))
	(tail (cdr parsed)))
    (cond ((eq head 'doc)
	   (dimwiki-justext tail))
	  ((eq head nil)
	   (when tail
	     (dimwiki-justext tail)))
	  ((listp head)
	   (cons (dimwiki-justext head)
		 (dimwiki-justext tail)))
	  ((memq head '(tmpl tmpl? ref))
	   nil)
	  ((memq head '(link link?))
	   (unless (dimwiki-skiplink tail)
	     (dimwiki-justext tail)))
	  ((stringp head)
	   (cons head (dimwiki-justext tail)))
	  (t parsed))))

(defun dimwiki-flatten (lol)
  (when lol
    (if (atom lol)
	(list lol)
      (append (dimwiki-flatten (car lol))
	      (dimwiki-flatten (cdr lol))))))



(defun dimwiki-maybe-redirect (parsed)
  "Return nil if no redirect, else new search term."
  (when (and (stringp (cadr parsed))
	     (member (upcase (replace-regexp-in-string " *$" "" (cadr parsed)))
		     dimwiki-redirect-keywords)
	     (eq 'link (car (cadr (cdr parsed))))
	     (stringp (cadr (cadr (cdr parsed)))))
    (cadr (cadr (cdr parsed)))))

(defun dimwiki-justext-of-parsed (parsed)
  (replace-regexp-in-string
   "^\\s +" ""
   (replace-regexp-in-string
    " *\n *\\|  +" " "
    (apply 'concat (dimwiki-flatten
		    (dimwiki-justext parsed))))))

(defun dimwiki-scatter-search-query (term)
  (let ((domains dimwiki-wikidomains)
	parsed article)
    (while (and domains (not parsed))
      (setq article (dimwiki-extract-article-from-json
		     (dimwiki-send-search-query term
						(car domains))))
      (if article
	  (progn (setq parsed (dimwiki-parse article))
		 (let ((redirect (dimwiki-maybe-redirect parsed)))
		   (when redirect
		     (setq term redirect
			   parsed nil))))
	(setq domains (cdr domains))))
    parsed))

(defun dimwiki-search (term)
  (interactive "sSearch term: ")
  (let ((results (dimwiki-justext-of-parsed
		  (dimwiki-scatter-search-query term))))
    (if (and results (not (equal "" results)))
	(message "%s" results)
      (message "No hits!"))
    results))

(provide 'dimwiki)
;;; dimwiki.el ends here
