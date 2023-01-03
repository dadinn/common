(define-module (common utils)
  #:export
  (getopt-extra usage config-filename
   read-config write-config write-config-vars
   parse-unit-as-bytes emit-bytes-as-unit
   parse-arg-alist emit-arg-alist
   root-user? block-device?
   executable? directory?
   println system->string* system->devnull*
   path mkdir-p move-file which*
   assoc-get hash-equal? unique group-by)
  #:use-module ((srfi srfi-1) #:prefix srfi1:)
  #:use-module ((srfi srfi-64))
  #:use-module ((ice-9 i18n) #:prefix i18n:)
  #:use-module ((ice-9 pretty-print) #:prefix pp:)
  #:use-module ((ice-9 getopt-long) #:prefix getopt:)
  #:use-module ((ice-9 hash-table) #:prefix hash:)
  #:use-module ((ice-9 rdelim) #:prefix rdelim:)
  #:use-module ((ice-9 regex) #:prefix regex:)
  #:use-module ((ice-9 popen) #:prefix popen:))

(define* (path head #:rest tail)
  (string-join (cons head tail) "/"))

(define (mkdir-p directory-path)
  "Create directory if it does not already exist, by creating parent directories if necessary."
  (srfi1:fold
   (lambda (dir parent)
     (let ((curr (if parent (path parent dir) dir)))
       (when (not (or (string-null? curr) (file-exists? curr)))
	 (mkdir curr))
       curr))
   #f (string-split directory-path #\/)))

(define* (assoc-get m #:rest keys)
  "Fetch values from nested alist or hash-table."
  (srfi1:fold
   (lambda (k acc)
     (and acc
      (if (hash-table? acc)
          (hash-ref acc k)
          (assoc-ref acc k))))
   m keys))

(define (hash-equal? h1 h2)
  "Recursively compare hast-table for equality."
  (cond
   ((and
     (hash-table? h1)
     (hash-table? h2))
    (hash-fold
     (lambda (key val1 result)
       (let ((val2 (hash-ref h2 key)))
         (and result (hash-equal? val1 val2))))
     #t h1))
   (else equal? h1 h2)))

(define (unique items)
  "Return list of unique items, preserving order."
  (let* ((size (exact-integer-sqrt (length items)))
         (cache (make-hash-table size)))
    (reverse
     (srfi1:fold
      (lambda (e acc)
        (cond
         ((not (hash-ref cache e))
          (hash-set! cache e #t)
          (cons e acc))
         (else acc)))
      '() items))))

(define (group-by key-fn items)
  "Aggregate items into a multi-map, keyed by the `key-fn` function called on each item."
  (let* ((size (exact-integer-sqrt (length items)))
	 (result (make-hash-table size)))
    (for-each
     (lambda (item)
       (let* ((key (key-fn item))
	      (acc (hash-ref result key '())))
	 (hash-set! result key (cons item acc))))
     items)
    result))

(define (block-device? path)
  (and (file-exists? path)
       (eq? 'block-special (stat:type (stat path)))))

(define (directory? path)
  (and (file-exists? path)
       (eq? 'directory (stat:type (stat path)))))

(define (executable? path)
  (and (file-exists? path)
       (access? path X_OK)))

(define (executable-on-path? file-name paths)
  (srfi1:fold
   (lambda (p res) (or res (executable? (path p file-name))))
   #f paths))

(define* (which* #:rest commands)
  "Given a list of commands, returns those which are not available on the PATH as executables."
  (let ((paths (string-split (getenv "PATH") #\:)))
    (srfi1:fold
     (lambda (command acc)
       (if (executable-on-path? command paths)
	   acc (cons command acc)))
     '() commands)))

(define (root-user?)
  (let* ((id-res (system->string* "id" "-u"))
	 (id-match (regex:string-match "[0-9]+" id-res))
	 (id-match (regex:match:substring id-match 0))
	 (id (string->number id-match)))
    (zero? id)))

(define* (println #:rest args)
  (display (string-join args " "))
  (newline))

(define* (system->string* #:rest args)
  (let* ((command (string-join args " "))
	 (in (popen:open-input-pipe command))
	 (text (rdelim:read-string in)))
    (popen:close-pipe in)
    text))

(define* (system->devnull* #:rest args)
  (with-error-to-file "/dev/null"
    (lambda ()
      (with-output-to-file "/dev/null"
	(lambda ()
	  (apply system* args))))))

(define supported-props
  (hash:alist->hash-table
   (map
    (lambda (k) (cons k #t))
    '(single-char value required? predicate))))

(define (conform-spec spec)
  (map
   (lambda (kv)
     (cons
      (car kv)
      (filter
       (lambda (kv) (hash-ref supported-props (car kv)))
       (cdr kv))))
   spec))

(define* (getopt-extra args options-spec #:optional defaults-override)
  (let* ((options (getopt:getopt-long args (conform-spec options-spec)))
	 (varargs (getopt:option-ref options '() #f))
	 (result (make-hash-table (length options-spec))))
    (for-each
     (lambda (spec)
       (let* ((long-name (car spec))
	      (props (cdr spec))
	      (default (assoc-ref props 'default))
	      (default (and default (car default)))
	      (default
		(if defaults-override
		    (hash-ref defaults-override long-name default)
		    default))
	      (value (getopt:option-ref options long-name default)))
	 (if value (hash-set! result long-name value))))
     options-spec)
    (when varargs
      (hash-set! result '() varargs))
    result))

(define (read-config path)
  (if (file-exists? path)
      (let* ((lr-file (open-input-file path))
	     (lr-alist
	      (map
	       (lambda (kv) (cons (car kv) (cadr kv)))
	       (read lr-file))))
	(close lr-file)
	(hash:alist->hash-table lr-alist))
      (make-hash-table 0)))

(define (write-config path options)
  (let ((lrfile (open-output-file path)))
    (pp:pretty-print
     (filter
      (lambda (entry)
	(not (equal? '() (car entry))))
      (hash-map->list list options)) lrfile)
    (close lrfile)))

(define (write-config-vars path options)
  (with-output-to-file path
    (lambda ()
      (map
       (lambda (entry)
	 (let* ((key (car entry))
		(key (symbol->string key))
		(key (i18n:string-locale-upcase key))
		(value (cadr entry))
		(value
		 (if (boolean? value)
		     (if value "1" "0")
		     value)))
	   (display (string-append key "=" value))
	   (newline)))
       (filter
	(lambda (entry)
	  (not (equal? '() (car entry))))
	(hash-map->list list options))))))

(define (move-file oldfile newfile)
  (copy-file oldfile newfile)
  (delete-file oldfile))

(define config-filename "INSTROOT_VARS.scm")

(define* (usage specs #:optional defaults-override)
  (string-join
   (map
    (lambda (spec)
      (let* ((long-name (car spec))
	     (props (cdr spec))
	     (props (map (lambda (spec) (cons (car spec) (cadr spec))) props))
	     (single-char (assoc-ref props 'single-char))
	     (description (assoc-ref props 'description))
	     (value (assoc-ref props 'value))
	     (value-arg (assoc-ref props 'value-arg))
	     (default (assoc-ref props 'default))
	     (default
	       (if defaults-override
		   (hash-ref defaults-override long-name default)
		   default)))
	(string-append
	 (if single-char (string #\- single-char #\space) "")
	 (string-append "--" (symbol->string long-name))
	 (cond
	  ((and value value-arg)
	   (string-append " " (i18n:string-locale-upcase value-arg)))
	  (value " ARG")
	  (else ""))
	 "\n"
	 (if description description "NO DESCRIPTION")
	 (cond
	  ((and default value)
	   (string-append " (default: " default ")"))
	  (default " (default)")
	  (else "")))))
    specs)
   "\n\n"))

(define unit-factors
  '(("" . 0)
    ("K" . 10)
    ("M" . 20)
    ("G" . 30)
    ("T" . 40)
    ("P" . 50)
    ("E" . 60)
    ("Z" . 70)
    ("Y" . 80)))

(define (parse-unit-as-bytes unit-string)
  (let* ((matches (regex:string-match "^([0-9]+)([KMGTPEZY]?)$" unit-string))
	 (amount (and matches (regex:match:substring matches 1)))
	 (amount (and amount (string->number amount)))
	 (unit (and matches (regex:match:substring matches 2)))
	 (factor (assoc-ref unit-factors unit)))
    (if matches (* amount (expt 2 factor))
     (error "Cannot parse as bytes:" unit-string))))

(define (match-unit-factor bytes units)
  (srfi1:fold
   (lambda (next current)
     (let ((next-factor (cdr next)))
       (if (< 0 (quotient bytes (expt 2 next-factor)))
	   next current)))
   (car units)
   (cdr units)))

(define (emit-bytes-as-unit bytes)
  (let* ((unit (match-unit-factor bytes unit-factors))
	 (unit-symbol (car unit))
	 (unit-factor (cdr unit)))
    (string-append
     (number->string
      (floor (/ bytes (expt 2 unit-factor))))
     unit-symbol)))

(define* (parse-arg-alist args-string
   #:key
   (pair-separator #\=)
   (list-separator #\,))
  (map
   (lambda (kv-string)
     (let* ((items (string-split kv-string pair-separator))
	    (size (length items)))
       (cond
	((eqv? size 1) (car items))
	((eqv? size 2) (cons (car items) (cadr items)))
	(else (cons (car items)
		    (string-join (cdr items) (string pair-separator)))))))
   (string-split args-string #\,)))

(define* (emit-arg-alist arg-alist
   #:key
   (pair-separator "=")
   (list-separator ","))
  (string-join
   (map
    (lambda (arg)
      (if (pair? arg)
       (string-append (car arg) pair-separator (cdr arg))
       arg))
    arg-alist)
   list-separator))

;;; TESTS

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
