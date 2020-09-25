;;; delve-data-types-test.el --- test for delve-data-types.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: test


(require 'buttercup)
(require 'delve-data-types)

(describe "Core Data Types:"
  (it "Distinguish data items by their type"
    (let* ((zettel (delve-make-zettel))
	   (tolink (delve-make-tolink))
	   (backlink (delve-make-backlink)))
      (expect (type-of zettel) :to-be 'delve-zettel)
      (expect (type-of tolink) :to-be 'delve-tolink)
      (expect (type-of backlink) :to-be 'delve-backlink)))
  (it "Check for result type of searches."
    (let* ((search-zettel (delve-make-search-for-zettel)))
      (expect (delve-search-for-zettel-result-subtype search-zettel)
	      :to-be 'delve-zettel))))

(provide 'delve-data-types-test)
;;; delve-data-types-test.el ends here
