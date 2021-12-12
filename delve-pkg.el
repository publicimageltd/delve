(define-package "delve" "0.9.3"
  "Delve into your org roam zettelkasten."
  '((emacs "27.2")
    ;; Lister tries to avoid dash, but since Org Roam uses it anyway:
    (dash "2.13")
    (transient "0.3.7")
    (org-roam "2.1")
    (lister "0.9.4"))
  :keywords '("org-roam" "hypermedia")
  :homepage "https://github.com/publicimageltd/delve")

