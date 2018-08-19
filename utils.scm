(define-module (common utils)
  #:export
  (get-lastrun write-lastrun getopt-lastrun usage
   println block-device? root-user? which*
   system->string* system->devnull*)
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

(define (block-device? path)
  (and (file-exists? path)
       (eq? 'block-special (stat:type (stat path)))))

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
  (srfi-1:fold
   (lambda (kv new-props)
     (if (hash-ref supported-props (car kv))
	 (cons kv new-props)
	 new-props))
   #nil
   props))

(define (conform-spec spec)
  (map
   (lambda (kv)
     (cons
      (car kv)
      (conform-props (cdr kv))))
   spec))

(define (get-lastrun path)
  (if (file-exists? path)
      (let* ((lr-file (open-input-file path))
	     (lr-alist
	      (map
	       (lambda (kv) (cons (car kv) (cadr kv)))
	       (read lr-file))))
	(close lr-file)
	(hash:alist->hash-table lr-alist))
      (make-hash-table 0)))

(define* (getopt-lastrun args options-spec #:optional lastrun-map)
  (let* ((options (getopt:getopt-long args (conform-spec options-spec)))
	 (result (make-hash-table (length options-spec))))
    (srfi-1:fold
     (lambda (spec noop!)
       (let* ((long-name (car spec))
	      (props (cdr spec))
	      (default (assoc-ref props 'default))
	      (default (and default (car default)))
	      (default (hash-ref lastrun-map long-name default))
	      (value (getopt:option-ref options long-name default)))
	 (if value (hash-set! result long-name value))))
     #nil options-spec)
    (let ((rest (getopt:option-ref options '() #f)))
      (if rest (hash-set! result '() rest)) result)))

(define (write-lastrun path options)
  (let ((lrfile (open-output-file ".lastrun")))
    (pp:pretty-print (hash-map->list list options) lrfile)
    (close lrfile)))

(define (usage specs lastrun)
  (string-join
   (map
    (lambda (spec)
      (let* ((long-name (car spec))
	     (props (cdr spec))
	     (single-char (assoc-ref props 'single-char))
	     (description (assoc-ref props 'description))
	     (value (assoc-ref props 'value))
	     (value-arg (assoc-ref props 'value-arg))
	     (default (assoc-ref props 'default))
	     (lastrun (hash-ref lastrun long-name)))
	(string-append
	 (if single-char
	     (string #\- (car single-char)))
	 " "
	 (string-append "--" (symbol->string long-name))
	 (if value
	     (if value-arg
		 (string-append " " (i18n:string-locale-upcase (car value-arg)) "\n")
		 " ARG\n")
	     "\n")
	 (if description (car description) "NO DESCRIPTION")
	 (if value
	     (cond
	      (lastrun (string-append " (default " lastrun ")"))
	      (default (string-append " (default " (car default) ")"))
	      (else ""))
	     (if (or lastrun default) " (default)" "")))))
    specs)
   "\n\n"))
