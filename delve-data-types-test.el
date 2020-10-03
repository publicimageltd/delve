;;; delve-data-types-test.el --- test for delve-data-types.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: test


(require 'buttercup)
(require 'delve-data-types)

(describe "Core Data Types:"
  (it "Distinguish data items by their type"
    (let* ((page     (delve-make-page))
	   (tolink   (delve-make-tolink))
	   (backlink (delve-make-backlink)))
      (expect (type-of page)     :to-be 'delve-page)
      (expect (type-of tolink)   :to-be 'delve-tolink)
      (expect (type-of backlink) :to-be 'delve-backlink)))
  (it "Check for result type of searches."
    (let* ((page-search (delve-make-page-search))) 
      (expect (delve-page-search-result-makefn page-search)
	      :to-be 'delve-make-page))))

(provide 'delve-data-types-test)
;;; delve-data-types-test.el ends here
