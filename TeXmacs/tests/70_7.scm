(use-modules
  (data cpp)
  (data java)
  (data julia)
  (data scala)
  (data python))

(define (regtest-cpp)
  (regression-test-group
   "cpp file suffix" "cpp format"
   format-from-suffix :none
   (test "*.cpp" "cpp" "cpp")
   (test "*.hh" "hh" "cpp")
   (test "*.hpp" "hpp" "cpp")
   (test "*.cc" "cc" "cpp")))

(define (regtest-scheme)
  (regression-test-group
   "scheme file suffix" "scheme format"
   format-from-suffix :none
   (test "*.scm" "scm" "scheme")))

(define (regtest-scala)
  (regression-test-group
   "scala file suffix" "scala format"
   format-from-suffix :none
   (test "*.scala" "scala" "scala")))

(define (regtest-java)
  (regression-test-group
   "java file suffix" "java format"
   format-from-suffix :none
   (test "*.java" "java" "java")))

(define (regtest-python)
  (regression-test-group
   "python file suffix" "python format"
   format-from-suffix :none
   (test "*.py" "py" "python")))

(define (regtest-format?)
  (regression-test-group
   "format?" "boolean"
   format? :none
   (test "python format" "python" #t)
   (test "scala format" "scala" #t)
   (test "no such format" "no-such-format" #f)))

(define (regtest-format-get-name)
  (regression-test-group
   "format-get-name" "string"
   format-get-name :none
   (test "python format" "python" "Python source code")
   (test "scala format" "scala" "Scala source code")
   (test "no such format" "no-such-format" #f)))

(define (regtest-format-from-suffix)
  (regression-test-group
   "format-from-suffix" "string"
   format-from-suffix :none
   (test "scheme format" "scm" "scheme")
   (test "python format" "py" "python")
   (test "java format" "java" "java")
   (test "scala format" "scala" "scala")
   (test "julia format" "jl" "julia")
   (test "cpp format" "cpp" "cpp")
   (test "cpp format" "hpp" "cpp")
   (test "cpp format" "cc" "cpp")
   (test "cpp format" "hh" "cpp")
   (test "texmacs format" "tm" "texmacs")
   (test "texmacs format" "ts" "texmacs")
   (test "texmacs format" "stm" "stm")
   (test "png format" "png" "png")
   (test "no such format" "no-such-format" "generic")))

(tm-define (test_70_7)
  (let ((n (+ (regtest-cpp)
              (regtest-java)
              (regtest-python)
              (regtest-scala)
              (regtest-scheme)
              (regtest-format?)
              (regtest-format-get-name)
              (regtest-format-from-suffix))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 70_7: ok\n")))
