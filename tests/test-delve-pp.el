;;; test-delve-pp.el --- Tests for delve-pp.el       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  

;; Author:  <joerg@joergvolbers.de>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Tests for delve-pp.el

;;; Code:

(require 'buttercup)
(require 'delve-pp)

;; * Utilities

(defface delve-pp-testface
  '((t (:weight bold)))
  "Face for testing pretty printing."
  :group 'testgroup)

(defun test-delve--string-properties (s)
  "Return a list of all properties of S, with char indices.
The list is simlar to the output of `prin1', with the string
itself ommitted."
  (let* ((s-prin1 (format "%S" s)))
    (if (string-prefix-p "#(" s-prin1)
	(cdr (read (substring s-prin1 1)))
      nil)))

(defun test-delve--merged-props (s)
  "Return a list of all properties of S, disregarding indices."
  (seq-uniq (cl-remove-if-not #'listp (test-delve--string-properties s))))

;; * The Tests

(describe "delve-pp-apply-mods"
  
  (it "returns string unmodified if no mod is passed"
    (let ((s "the string"))
      (expect (delve-pp-apply-mods s nil nil)
	      :to-equal s)))
  
  (it "sets face property when when using mod (:set-face facename)"
    (let ((s "the string")
	  (face 'delve-pp-testface))
      (expect (test-delve--merged-props (delve-pp-apply-mods s :set-face face))
	      :to-equal
	      '((face delve-pp-testface)))))
  
  (it "appends face when using mod (:add-face facename)"
    (let* ((s    "the string")
	   (s-propped (propertize s 'face 'first-face))
	   (s-result (delve-pp-apply-mods s-propped
					  :add-face 'second-face)))
      (expect (test-delve--merged-props s-result)
	      :to-equal
	      '((face (first-face second-face))))))

  (it "does not add face when delve-pp-inhibit-faces is set"
    (let ((s "the string")
	  (face 'delve-pp-testface)
	  (delve-pp-inhibit-faces t))
      (expect (delve-pp-apply-mods s :set-face face)
	      :to-equal s)))
  
  (it "pads string with extra whitespaces using mod (:width n)"
    (let ((s "the string"))
      (expect (length (delve-pp-apply-mods s :width 30))
	      :to-be  30)))
  
  (it "shortens long string using mod (:width n)"
    (let ((s "the very very very long string which is insanely long oh my god oh my gosh"))
      (expect (length (delve-pp-apply-mods s :width 30))
	      :to-be 30)))
  
  (it "returns string unmodified using unknown keyword (:nomod n)"
    (let ((s "the string"))
      (expect (delve-pp-apply-mods s :nomod :nomod)
		:to-equal s))))

(describe "delve-pp-item"
  
  (describe "basic calling variants"
    (it "returns a string if it is passed as a pprinter argument"
      (let ((s "the string"))
	(expect (delve-pp-item nil s nil)
		:to-equal s)))
    
    (it "calls the pprinter function with the object"
      (let ((s "this is my result"))
	(expect (delve-pp-item s #'identity nil)
		:to-equal s))))

  (describe "using modifiers"
    (it "passes the mod keyword and its args to the mod application function"
      (spy-on 'delve-pp-apply-mods)
      (let ((s "the string"))
	(delve-pp-item nil "the string" '(:set-face delve-pp-testface))
	(expect 'delve-pp-apply-mods
		:to-have-been-called-with s :set-face 'delve-pp-testface)))
    
    (it "omits a second iteration on :format if pprinter was not a function"
      (spy-on 'delve-pp-apply-mods :and-call-fake
	      (lambda (s &rest _)
		(concat "." s)))
      (let* ((orig-s "the-string")
	     (new-s (delve-pp-item nil orig-s '(:set-face delve-pp-testface
							  :width 30
							  :format "%s"))))
	(expect 'delve-pp-apply-mods
		:to-have-been-called-times 2)
	(expect new-s :to-equal (concat ".." orig-s))))

    (it "iterates over pairs of mod keywords and arguments"
      (spy-on 'delve-pp-apply-mods :and-call-fake
	      (lambda (s &rest _)
		(concat "." s)))
      (let* ((orig-s "the-string")
	     (new-s (delve-pp-item nil orig-s
				   '(:set-face delve-pp-testface
					   :width 30
					   :add-face 'another-face))))
	(expect 'delve-pp-apply-mods
		:to-have-been-called-times 3)
	(expect new-s :to-equal (concat "..." orig-s))))))

(describe "delve-pp-line"
  (it "can be used to just concenate stringss"
    (let* ((s1 "the")
	   (s2 "string")
	   (pp-scheme (list s1 s2)))
      (expect (delve-pp-line nil pp-scheme "")
	      :to-equal
	      (concat s1 s2))))
  (it "joins the results from unmodified pretty printer with space"
    (let* ((obj "the string")
	   (pp-scheme '(identity identity)))
      (expect (delve-pp-line obj pp-scheme)
	      :to-equal
	      (concat obj " " obj))))
  (it "accepts mod-arg-pairs in two different formats"
    (let* ((obj "the object")
	   (pp-scheme '((identity :width 30)
			(identity (:set-face some-face)))))
      (spy-on 'delve-pp-item)
      (delve-pp-line obj pp-scheme)
      (expect 'delve-pp-item :to-have-been-called-times 2)))
  
  (it "returns error string when scheme is invalid"
    (let* ((obj "the object")
	   (pp-scheme '((identity :something)
			(identity :aaargh))))
      (expect (delve-pp-line obj pp-scheme)
	      :to-equal
	      (string-join
	       (mapcar (apply-partially
			#'format delve-pp-invalid-scheme-error-string)
		       pp-scheme)
	       " "))))
  
  (it "returns nil when scheme is invalid and  error string is set to nil"
    (let* ((obj "the object")
	   (pp-scheme '((identity :something)))
	   (delve-pp-invalid-scheme-error-string nil))
      (expect (delve-pp-line obj pp-scheme)
	      :to-equal ""))))

(provide 'test-delve-pp)
;;; test-delve-pp.el ends here
