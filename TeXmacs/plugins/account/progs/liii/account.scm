(texmacs-module (liii account))

(tm-define (account-save-token token)
  (display "OAuth2 Token: ")
  (display token)
  (newline))
