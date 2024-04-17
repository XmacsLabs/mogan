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


(tm-define (test_70_7)
  (let ((n (+ (regtest-cpp)
              (regtest-java)
              (regtest-python)
              (regtest-scala)
              (regtest-scheme))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of prog-format: ok\n")))
