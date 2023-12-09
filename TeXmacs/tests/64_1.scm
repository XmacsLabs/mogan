(define (file-under-path path)
  (lambda (file)
    (url-concretize (url-append path file))))

(define embedded-propose-4th
  (lambda (tree)
    (embedded-propose tree 4)))

(define (test-ascii-filename)
  (load-buffer "$TEXMACS_PATH/tests/tm/64_1.tm")
  (let ((image-ascii (tree-ref (root-tree) 1 1))
        (image-untitled (tree-ref (root-tree) 1 2))
        (image-cork (tree-ref (root-tree) 1 3)))
    (regression-test-group
      "embedded-propose on latin filename" "embedded-propose-latin"
      embedded-propose-4th (file-under-path "$TEXMACS_PATH/tests/tm")
      (test "cork encoding" image-cork "学生：张佳.png")
      (test "ascii"  image-ascii "64_1.png")
      (test "extension only" image-untitled "64_1-image-4.png"))))

(define (test-cjk-filename)
  (load-buffer "$TEXMACS_PATH/tests/tm/64_1_中文文件名.tm")
  (let ((image-ascii (tree-ref (root-tree) 1 1))
        (image-untitled (tree-ref (root-tree) 1 2))
        (image-cork (tree-ref (root-tree) 1 3)))
    (regression-test-group
      "embedded-propose on unicode filename" "embedded-propose-unicode"
      embedded-propose-4th (file-under-path "$TEXMACS_PATH/tests/tm")
      (test "cork encoding" image-cork "学生：张佳.png")
      (test "ascii"  image-ascii "64_1.png")
      (test "extension only" image-untitled "64_1_中文文件名-image-4.png"))))

(tm-define (test_64_1)
  (let ((n (+ (test-ascii-filename)
              (test-cjk-filename))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 64_1: ok\n")))
