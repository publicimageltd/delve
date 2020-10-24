;;; delve-test.el --- Tests for delve                -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <joerg@joergvolbers.de>

;;; Code:

(require 'buttercup)
(require 'delve)

(describe "Expanding:"
  (it "delve-expand: Collect the results in a list"
    (let ((res (delve-expand "ITEM" #'identity #'list #'identity)))
      (expect res :to-equal '("ITEM" "ITEM" "ITEM")))))

(provide 'delve-test)
;;; delve-test.el ends here
