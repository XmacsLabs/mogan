(set! *load-path*
  (cons (append (get-env "TEXMACS_HOME_PATH") "plugins/s7/s7")
        *load-path*))

(define (s7-welcome)
  (display "S7 Scheme: ")
  (display (substring (*s7* 'version) 3))
  (newline))