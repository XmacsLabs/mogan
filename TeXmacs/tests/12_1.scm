(define (get-tag-name bib_file)
 (with bib (tree->stree (parse-bib (string-load bib_file)))
  (bib-car (bib-cdr (bib-cdr (bib-car (bib-cdr bib)))))))

(define (test-get-tag-name)
  (regression-test-group
   "get-tag-name" "tag-name"
   get-tag-name utf8->cork 
   (test "get-tag-name" "$TEXMACS_PATH/tests/bib/12_1.bib" "数理逻辑2010汪芳庭")))

(tm-define (test_12_1)
  (let ((n (+ (test-get-tag-name))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 12_1: ok\n")))
