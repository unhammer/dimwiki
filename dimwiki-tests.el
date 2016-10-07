;;; dimwiki-tests.el --- tests for dimwiki.el

;; Copyright (C) 2014-2016  Kevin Brubeck Unhammer <unhammer@fsfe.org>

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

;; Tests

;;; Code:

(require 'ert)
(load-file "./dimwiki.el")
(require 'dimwiki)

(ert-deftest dimwiki--parse ()
  :tags '(dimwiki)
  (should (equal (dimwiki--parse "foo {{fie blah}} bar")
		 '(doc "foo "
		       (tmpl "fie blah")
		       " bar")))
  (should (equal (dimwiki--parse "foo [[fie blah]] bar")
		 '(doc "foo "
		       (link "fie blah")
		       " bar")))
  (should (equal (dimwiki--parse "foo {{fie[[with ]]blah}} bar")
		 '(doc "foo "
		       (tmpl "fie" (link "with ") "blah")
		       " bar")))
  (should (equal (dimwiki--parse "foo '''meh''' bar")
		 '(doc "foo "
		       "'''"		; too stupid to parse these :-)
		       "meh"
		       "'''"
		       " bar")))
  (should (equal (dimwiki--parse "foo [[ {{fie [[fum foo]] blah}} ]] bar")
		 '(doc "foo "
		       (link " "
			     (tmpl "fie "
				   (link "fum foo")
				   " blah")
			     " ")
		       " bar")))
  (should (equal (dimwiki--parse "foo [[fie|blah]] bar")
		 '(doc "foo "
		       (link (ref "fie") "blah")
		       " bar"))))

(defun dimwiki--run-tests ()
  (ert-run-tests-interactively 'dimwiki--parse))


(provide 'dimwiki-tests)
;;; dimwiki-tests.el ends here
