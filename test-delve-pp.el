;;; test-delve-pp.el --- Tests for delve-pp.el       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: 

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

;;; Commentary:

;; Tests for delve-pp.el

;;; Code:

(require 'buttercup)
(require 'delve-pp)

(defface delve-pp-testface
  '((t (:weight bold)))
  "Face for testing pretty printing."
  :group 'testgroup)

(describe "delve-pp-apply-mods"
  (it "returns string unmodified if no mod is passed"
    (let ((s "the string"))
      (expect (delve-pp-apply-mods s nil nil)
	      :to-equal s)))
  (it "returns propertized string when using mod (:face facename)"
    (let ((s "the string")
	  (face 'delve-pp-testface))
      (expect (delve-pp-apply-mods s :face face)
	      :to-equal
	      (propertize s 'face face))))
  (it "pads string with extra whitespaces using mod (:width n)"
    (let ((s "the string"))
      (expect (length (delve-pp-apply-mods s :width 30))
	      :to-be
	      30)))
  (it "shortens long string using mod (:width n)"
    (let ((s "the very very very long string which is insanely long oh my god oh my gosh"))
      (expect (length (delve-pp-apply-mods s :width 30))
	      :to-be
		30)))
  (it "returns string unmodified using unknown keyword (:nomod n)"
    (let ((s "the string"))
      (expect (delve-pp-apply-mods s :nomod :nomod)
		:to-equal
		s))))

(describe "delve-pp-item"
  
  (describe "basic calling variants"
    (it "returns a string if it is passed as a pprinter argument"
      (let ((s "the string"))
	(expect (delve-pp-item nil s nil)
		:to-equal
		s)))
    (it "calls the pprinter function with the object"
      (let ((s "this is my result"))
	(fset 'test-pp (lambda (object) object))
	(expect (delve-pp-item s 'test-pp nil)
		:to-equal
		s))))

  (describe "using modifiers"
    (it "passes the mod keyword and its args to the mod application function"
      (spy-on 'delve-pp-apply-mods)
      (let ((s "the string"))
	(delve-pp-item nil "the string" '(:face delve-pp-testface))
	(expect 'delve-pp-apply-mods
		:to-have-been-called-with
		s
		:face
		'delve-pp-testface)))
    (it "iterates over pairs of mod keywords and arguments"
      (spy-on 'delve-pp-apply-mods :and-call-fake
	      (lambda (s &rest _)
		(concat "." s)))
      (let* ((orig-s "the-string")
	     (new-s (delve-pp-item nil orig-s
				   '(:face delve-pp-testface
					   :width 30
					   :format "%s"))))
	(expect 'delve-pp-apply-mods
		:to-have-been-called-times 3)
	(expect new-s :to-equal (concat "..." orig-s))))))



(provide 'test-delve-pp)
;;; test-delve-pp.el ends here
