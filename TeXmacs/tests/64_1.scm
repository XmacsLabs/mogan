(define (file-under-path path)
  (lambda (file)
    (url-concretize (url-append path file))))

(define embedded-propose-4th
  (lambda (tree)
    (embedded-propose tree 4)))

(define (test-ascii-filename)
  (load-buffer "$TEXMACS_PATH/tests/64_1.tm")
  (let ((image-ascii (tree-ref (root-tree) 1 1))
        (image-untitled (tree-ref (root-tree) 1 2))
        (image-cork (tree-ref (root-tree) 1 3)))
    (regression-test-group
      "embedded-propose on latin filename" "embedded-propose-latin"
      embedded-propose-4th (file-under-path "$TEXMACS_PATH/tests")
      (test "cork encoding" image-cork "学生：张佳.png")
      (test "ascii"  image-ascii "64_1.png")
      (test "extension only" image-untitled "64_1-image-4.png"))))

(define (test-cjk-filename)
  (load-buffer "$TEXMACS_PATH/tests/64_1_中文文件名.tm")
  (let ((image-ascii (tree-ref (root-tree) 1 1))
        (image-untitled (tree-ref (root-tree) 1 2))
        (image-cork (tree-ref (root-tree) 1 3)))
    (regression-test-group
      "embedded-propose on unicode filename" "embedded-propose-unicode"
      embedded-propose-4th (file-under-path "$TEXMACS_PATH/tests")
      (test "cork encoding" image-cork "学生：张佳.png")
      (test "ascii"  image-ascii "64_1.png")
      (test "extension only" image-untitled "64_1_中文文件名-image-4.png"))))

(define (test-embedded-suffix)
  (regression-test-group
   "embedded edit, test tree with cork" "embedded-edit"
   embedded-suffix :none
   (test "cork encoding" '(image (tuple (raw-data "dummy") "<#672A><#547D><#540D><#7ED8><#56FE>.svg") "100pt" "80pt" "" "")
        "svg")
   (test "ascii" '(image (tuple (raw-data "dummy") "filename.png") "100pt" "80pt" "" "")
        "png")
   (test "extension only" '(image (tuple (raw-data "dummy") "png") "100pt" "80pt" "" "")
        "png")))

(tm-define (test_64_1)
  (let ((n (+ (test-ascii-filename)
              (test-cjk-filename)
              (test-embedded-suffix))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 64_1: ok\n")))
