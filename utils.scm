(define-module (common utils)
  #:export
  (read-lastrun parse-pairs
   write-lastrun write-lastrun-vars getopt-extra usage
   println block-device? directory? root-user?
   parse-unit-as-bytes emit-bytes-as-unit
   which* path system->string* system->devnull*)
  #:use-module ((srfi srfi-1) #:prefix srfi-1:)
  #:use-module ((ice-9 i18n) #:prefix i18n:)
  #:use-module ((ice-9 pretty-print) #:prefix pp:)
  #:use-module ((ice-9 getopt-long) #:prefix getopt:)
  #:use-module ((ice-9 hash-table) #:prefix hash:)
  #:use-module ((ice-9 rdelim) #:prefix rdelim:)
  #:use-module ((ice-9 regex) #:prefix regex:)
  #:use-module ((ice-9 popen) #:prefix popen:))

(define (which acc args)
  (if (not (null? args))
      (let ((curr (car args)))
	(if (zero? (system->devnull* "which" curr))
	    (which acc (cdr args))
	    (which (cons curr acc) (cdr args))))
      acc))

(define* (which* #:rest args)
  (with-output-to-file "/dev/null"
    (lambda () (which #nil args))))

(define* (path head #:rest tail)
  (string-join (cons head tail) "/"))

(define (block-device? path)
  (and (file-exists? path)
       (eq? 'block-special (stat:type (stat path)))))

(define (directory? path)
  (and (file-exists? path)
       (eq? 'directory (stat:type (stat path)))))

(define (root-user?)
  (let* ((id-res (system->string* "id" "-u"))
	 (id-match (regex:string-match "[0-9]+" id-res))
	 (id-match (regex:match:substring id-match 0))
	 (id (string->number id-match)))
    (zero? id)))

(define* (println #:rest args)
  (newline)
  (display (string-join args " "))
  (newline))

(define* (system->string* #:rest args)
  (let* ((command (string-join args " "))
	 (in (popen:open-input-pipe command))
	 (text (rdelim:read-string in)))
    (close in)
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

(define (conform-props props)
  (filter
   (lambda (kv) (hash-ref supported-props (car kv)))
   props))

(define (conform-spec spec)
  (map
   (lambda (kv)
     (cons
      (car kv)
      (conform-props (cdr kv))))
   spec))

(define (read-lastrun path)
  (if (file-exists? path)
      (let* ((lr-file (open-input-file path))
	     (lr-alist
	      (map
	       (lambda (kv) (cons (car kv) (cadr kv)))
	       (read lr-file))))
	(close lr-file)
	(hash:alist->hash-table lr-alist))
      (make-hash-table 0)))

(define* (getopt-extra args options-spec #:optional defaults-override)
  (let* ((options (getopt:getopt-long args (conform-spec options-spec)))
	 (varargs (getopt:option-ref options '() #f))
	 (result (make-hash-table (length options-spec))))
    (map
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

(define (write-lastrun path options)
  (let ((lrfile (open-output-file path)))
    (pp:pretty-print
     (filter
      (lambda (entry)
	(not (equal? '() (car entry))))
      (hash-map->list list options)) lrfile)
    (close lrfile)))

(define (write-lastrun-vars path options)
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
	 (if single-char
	     (string #\- single-char))
	 " "
	 (string-append "--" (symbol->string long-name))
	 (if value
	     (if value-arg
		 (string-append " " (i18n:string-locale-upcase value-arg) "\n")
		 " ARG\n")
	     "\n")
	 (if description description "NO DESCRIPTION")
	 (if default
	     (if value
		 (string-append " (default " default ")")
		 " (default)")
	     ""))))
    specs)
   "\n\n"))

(define (parse-pairs pair-list)
  (map (lambda (pair) (string-split pair #\:))
       (string-split pair-list #\,)))

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

(define (parse-unit-as-bytes size-unit)
  (let
      ((size-matcher (regex:string-match "^([0-9]+)([KMGTPEZY]?)$" size-unit)))
    (if size-matcher
	(let* ((size-num (regex:match:substring size-matcher 1))
	       (size-num (string->number size-num))
	       (size-unit (regex:match:substring size-matcher 2))
	       (unit-factor (assoc-ref unit-factors size-unit)))
	  (* size-num (expt 2 unit-factor)))
	(error "Cannot parse as bytes:" size-string))))

(define (match-unit-factor bytes units)
  (srfi-1:fold
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
