(define-package "delve" "0.9.4"
  "Delve into your Org Roam zettelkasten."
  '((emacs "27.2")
    ;; Lister tries to avoid dash, but since Org Roam uses it anyway:
    (dash "2.20")
    (transient "0.3.7")
    (org-roam "2.2.2")
    (lister "0.9.4"))
  :keywords '("org-roam" "hypermedia")
  :homepage "https://github.com/publicimageltd/delve")

