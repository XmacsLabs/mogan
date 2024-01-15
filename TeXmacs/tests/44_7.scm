
(define (test-unmodified)
  (if (== (git-status-file "LICENSE" (url-pwd)) "  ")
    1
    (error "short status of LICENSE should be '  ' but not" st)))

(define (test-git-deleted)
  (system "git rm LICENSE")
  (with st (git-status-file "LICENSE" (url-pwd))
    (system "git reset HEAD LICENSE")
    (system "git checkout LICENSE")
    (if (== st "D ")
      1
      (error "short status of LICENSE should be 'D ' but not " st))))

(define (test-file-deleted)
  (system-remove "LICENSE")
  (with st (git-status-file "LICENSE" (url-pwd))
    (system "git checkout LICENSE")
    (if (== st " D")
      1
      (error "short status of LICENSE should be ' D' but not " st))))

(define (test-file-ignored)
  (system-copy "LICENSE" "LICENSE.log")
  (with st (git-status-file "LICENSE.log" (url-pwd))
    (system-remove "LICENSE.log")
    (if (== st "!!")
      1
      (error "short status of LICENSE should be '!!' but not " st))))

(define (test-git-added)
  (system-copy "LICENSE" "LICENSE.add")
  (system "git add LICENSE.add")
  (with st (git-status-file "LICENSE.add" (url-pwd))
    (system "git rm -f LICENSE.add")
    (if (== st "A ")
      1
      (error "short status of LICENSE should be 'A ' but not " st))))

(define (test-file-added)
  (system-copy "LICENSE" "LICENSE.add")
  (with st (git-status-file "LICENSE.add" (url-pwd))
    (system-remove "LICENSE.add")
    (if (== st "??")
      1
      (error "short status of LICENSE should be '??' but not " st))))

; git-status-file cannot detect if a file is renamed
(define (test-git-renamed)
  (system "git mv LICENSE LICENSE.renamed")
  (with st (git-status-file "LICENSE.renamed" (url-pwd))
    (system "git mv LICENSE.renamed LICENSE")
    (if (== st "A ")
      1
      (error "short status of LICENSE.renamed should be 'A ' but not " st))))

(define (test-git-modified)
  (system-copy "xmake.lua" "LICENSE")
  (system "git add LICENSE")
  (with st (git-status-file "LICENSE" (url-pwd))
    (system "git reset HEAD LICENSE")
    (system "git checkout LICENSE")
    (if (== st "M ")
      1
      (error "short status of LICENSE.renamed should be 'M ' but not " st))))

(define (test-file-modified)
  (system-copy "xmake.lua" "LICENSE")
  (with st (git-status-file "LICENSE" (url-pwd))
    (system "git checkout LICENSE")
    (if (== st " M")
      1
      (error "short status of LICENSE.renamed should be ' M' but not " st))))

(define (test-git-file-modified)
  (system-copy "xmake.lua" "LICENSE")
  (system "git add LICENSE")
  (system-copy "README.md" "LICENSE")
  (with st (git-status-file "LICENSE" (url-pwd))
    (system "git reset HEAD LICENSE")
    (system "git checkout LICENSE")
    (if (== st "MM")
      1
      (error "short status of LICENSE.renamed should be 'MM' but not " st))))

(define (test-unexist)
  (with st (git-status-file "not_exist" (url-pwd))
    (if (== st "")
      1
      (error "short status of not_exist should be '' but not " st))))

(tm-define (test_44_7)
  (let ((n (+ (test-unmodified)
              (test-git-deleted)
              (test-file-deleted)
              (test-file-ignored)
              (test-git-added)
              (test-file-added)
              (test-git-renamed)
              (test-git-modified)
              (test-file-modified)
              (test-git-file-modified)
              (test-unexist))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of 44_7: ok\n")))
