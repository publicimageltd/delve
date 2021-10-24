(define-package "delve" "0.9.2"
  "Delve into your org roam zettelkasten."
  '((emacs "27.1")
    ;; Lister tries to avoid dash, but since Org Roam uses it anyway:
    (dash "2.13")
    (org-roam "2.1")
    (lister "0.9.1"))
  :keywords '("org-roam" "hypermedia")
  :homepage "https://github.com/publicimageltd/delve")

