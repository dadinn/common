
(add-to-load-path
 (dirname (dirname (current-filename))))

(use-modules
 ((common utils))
 ((srfi srfi-64)))

(set! test-log-to-file #f)

(let ((test-alist '(("foo" . (("bar" . 42) ("baz" . 13))) (#:fizz . #:buzz)))
      (test-table
       (let ((result (make-hash-table 5))
             (foo (make-hash-table 5)))
         (hash-set! foo "bar" 42)
         (hash-set! foo "baz" 13)
         (hash-set! result "foo" foo)
         (hash-set! result #:fizz #:buzz)
         result)))
  (test-begin "assoc-get")
  (test-eqv 42 (assoc-get test-alist "foo" "bar"))
  (test-eqv 42 (assoc-get test-table "foo" "bar"))
  (test-eqv 13 (assoc-get test-alist "foo" "baz"))
  (test-eqv 13 (assoc-get test-table "foo" "baz"))
  (test-eq #:buzz (assoc-get test-alist #:fizz))
  (test-eq #:buzz (assoc-get test-table #:fizz))
  (test-end "assoc-get"))

(let ((expected (make-hash-table 5))
      (nested (make-hash-table 5))
      (result (make-hash-table 5)))
  (hash-set! expected "A" 13)
  (hash-set! expected "B" 17)
  (hash-set! expected "C" nested)
  (hash-set! result "A" 13)
  (hash-set! result "B" 17)
  (hash-set! result "C" nested)
  (hash-set! nested "D" 23)
  (test-begin "hash-equal?")
  (test-assert "nested hash equality"
    (hash-equal? expected result))
  (test-end "hash-equal?"))

(test-begin "unique")
(test-equal "duplicates are removed"
  '("A" "B" "C")
  (unique '("A" "A" "B" "B" "B" "C")))
(test-equal "preserves left-to-right order"
  '("A" "B" "C")
  (unique '("A" "B" "B" "A" "C" "B")))
(test-end "unique")

(let ((employees
       '(((#:name . "John") (#:city . "London"))
         ((#:name . "John") (#:city . "London"))
         ((#:name . "Paul") (#:city . "London"))
         ((#:name . "Francois") (#:city . "Paris"))
         ((#:name . "Sebastien") (#:city . "Paris"))
         ((#:name . "Kentaro") (#:city . "Tokyo"))))
      (expected1 (make-hash-table 5))
      (expected2 (make-hash-table 5)))
  (test-begin "group-by")
  (hash-set! expected1 0 '(3 6 9 12))
  (hash-set! expected1 1 '(4 7 10 13))
  (hash-set! expected1 2 '(5 8 11 14))
  (hash-set! expected2 "London" '("John" "Paul"))
  (hash-set! expected2 "Paris" '("Francois" "Sebastien"))
  (hash-set! expected2 "Tokyo" '("Kentaro"))
  (test-assert "group by modulo"
      (hash-equal?
       (group-by
        '(3 4 5 6 7 8 9 10 11 12 13 14)
        (lambda (n)
          (let ((res (modulo n 3)))
            res)))
       expected1))
  (test-assert "group people's names by city"
      (hash-equal?
       (group-by
        employees
        (lambda (e) (assoc-ref e #:city))
        (lambda (e) (assoc-ref e #:name)))
       expected2))
  (test-end "group-by"))

(test-begin "parse-version")
(test-equal "major version only"
  '(5 #f #f #f)
  (parse-version "5"))
(test-equal "major and minor versions only"
  '(5 10 #f #f)
  (parse-version "5.10"))
(test-equal "major, minor, patch versions, without release tag"
  '(5 10 0 #f)
  (parse-version "5.10.0"))
(test-equal "major, minor, patch versions, with release tag"
  '(5 10 0 "21-amd64")
  (parse-version "5.10.0-21-amd64"))
(test-equal "only patch version missing"
  '(5 10 #f "foobar")
  (parse-version "5.10-foobar"))
(test-assert "invalid major version"
  (not (parse-version "X.0.9-meh")))
(test-end "parse-version")
