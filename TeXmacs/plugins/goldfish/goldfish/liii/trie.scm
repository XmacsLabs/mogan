; Copyright (c) 2018, Moritz Heidkamp
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are
; met:
;
; Redistributions of source code must retain the above copyright
; notice, this list of conditions and the following disclaimer.
;
; Redistributions in binary form must reproduce the above copyright
; notice, this list of conditions and the following disclaimer in the
; documentation and/or other materials provided with the distribution.
;
; Neither the name of the author nor the names of its contributors may
; be used to endorse or promote products derived from this software
; without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR)
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGE.

(define-library (liii trie)

  (export make-trie
          trie?
          trie-insert!
          trie-ref
          trie-ref*
          trie-value
          trie->list)
  (import (srfi srfi-1)
          (srfi srfi-2)
          (srfi srfi-9)
          (liii alist))

  (begin
    (define-record-type :trie
      (make-trie* children value)
      trie?
      (children trie-children trie-children-set!)
      (value trie-value trie-value-set!))

    (define (make-trie)
      (make-trie* (list) (list)))

    (define (trie-ref* trie key)
      (alist-ref/default (trie-children trie) key #f))

    (define* (trie-ref trie key (default #f))
      (let loop ((node trie)
                 (key key))
        (if (null? key)
          (if (null? (trie-value node))
            default
            (car (trie-value node)))
          (let ((child (trie-ref* node (car key))))
            (if child
              (loop child (cdr key))
              default)))))

    (define (add-child! trie key child)
      (trie-children-set!
        trie (alist-cons key child (trie-children trie))))

    (define (trie-insert! trie key val)
      (let loop ((node trie)
                 (key key))
        (if (null? key)
          (trie-value-set! node (list val))
          (let* ((ckey (car key))
                 (child (or (trie-ref* node ckey)
                            (let ((child (make-trie)))
                              (add-child! node ckey child)
                              child))))
            (loop child (cdr key))))))

    (define (trie->list trie)
      (cons
        (let loop ((trie trie))
          (map (lambda (child)
                 (cons (car child)
                       (trie->list (cdr child))))
               (trie-children trie)))
        (trie-value trie)))

    ) ; end of begin
  ) ; end of library
