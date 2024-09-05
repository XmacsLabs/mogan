
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : liii-keyword.scm
;; DESCRIPTION : keywords for the Goldfish Scheme Language
;; COPYRIGHT   : (C) 2024  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code liii-keyword))

(define (liii-base)
  (map symbol->string
    '(== != display* in? compose identity)))

(define (liii-check)
  (map symbol->string
    '(test check check-set-mode! check-catch check-report
      check-failed? check-true check-false)))

(define (liii-list)
  (map symbol->string
    '(list-view flatmap list-null? list-not-null? not-null-list?)))

(define (liii-os)
  (map symbol->string
    '(os-arch os-type os-windows? os-linux? os-macos? os-temp-dir
      os-call system mkdir chdir rmdir getenv unsetenv getcwd
      listdir access getlogin getpid)))

(define (liii-queue)
  (map symbol->string
    '(queue queue? queue-empty? queue-size queue-front queue-back
      queue-pop! queue-push! queue->list)))

(define (liii-stack)
  (map symbol->string
    '(stack stack? stack-empty? stack-size stack-top
      stack-push! stack-pop! stack->list)))

(define (liii-sys)
  (map symbol->string '(argv)))

(define (liii-uuid)
  (map symbol->string '(uuid4)))


(tm-define (liii-keywords)
  `(,@(liii-base) ,@(liii-check) ,@(liii-list) ,@(liii-os) ,@(liii-queue)
    ,@(liii-stack) ,@(liii-sys) ,@(liii-uuid)))

(tm-define (liii-keywords-define)
  (map symbol->string
    '(let1 typed-lambda defined? define-macro define-constant
      autoload require provide define* lambda* eval-string case*)))

(tm-define (liii-keywords-error)
  (map symbol->string
    '(os-error file-not-found-error not-a-directory-error file-exists-error
      timeout-error type-error value-error ??? not-implemented-error)))
