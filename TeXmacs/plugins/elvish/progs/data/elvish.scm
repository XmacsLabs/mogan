
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : data/elvish.scm
;; DESCRIPTION : Elvish data format
;; COPYRIGHT   : (C) 2024  Liii Network Inc
;; All rights reverved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (data elvish))

(define-format elvish
  (:name "Elvish source code")
  (:suffix "elv"))
  
(define (texmacs->elvish x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (elvish->texmacs x . opts)
  (code->texmacs x))

(define (elvish-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree elvish-document
  (:function texmacs->elvish))

(converter elvish-document texmacs-tree
  (:function elvish->texmacs))
  
(converter texmacs-tree elvish-snippet
  (:function texmacs->elvish))

(converter elvish-snippet texmacs-tree
  (:function elvish-snippet->texmacs))

