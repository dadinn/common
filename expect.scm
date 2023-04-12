(define-module (common expect)
  #:export (expect expect-strings interact))

(use-modules
 (system syntax)
 ((ice-9 threads) #:select
  (call-with-new-thread thread-exited?))
 ((ice-9 popen) #:prefix popen:)
 ((ice-9 regex) #:prefix rx:)
 ((ice-9 format) #:select (format))
 ((ice-9 readline) #:select (readline))
 ((srfi srfi-1) #:select (any))
 (srfi srfi-64))

(define (expect-select port timeout)
  (let* ((secs-usecs (gettimeofday))
         (relative
          (- timeout
             (car secs-usecs)
             (/ (cdr secs-usecs)
                ;; one million.
                1000000))))
    (and (> relative 0)
         (pair?
          (car
           (select
            (list port)
            '() '()
            relative))))))

(define-syntax expect-with-bindings
  (lambda (stx)
    (syntax-case stx (=>)

      ((expect-with-bindings
        (predicate-bindings ...)
        (clauses-with-bindings ...)
        ((predicate => procedure) more-clauses ...)
        other-params)
       #'(expect-with-bindings
          (predicate-bindings
           ...
           (predicate-binding predicate))
          (clauses-with-bindings
           ...
           (predicate-binding => (lambda (result) (apply procedure result))))
          (more-clauses ...)
          other-params))

      ((expect-with-bindings
        (predicate-bindings ...)
        (clauses-with-bindings ...)
        ((predicate body ...) more-clauses ...)
        other-params)
       #'(expect-with-bindings
          (predicate-bindings
           ...
           (predicate-binding predicate))
          (clauses-with-bindings
           ...
           (predicate-binding body ...))
          (more-clauses ...)
          other-params))

      ((expect-with-bindings
        (predicate-bindings ...)
        ((predicate-binding body ...) ...)
        () ; no more clauses
        ;; other parameters
        (expect-port expect-eof-proc
         expect-char-proc expect-pass-char?
         expect-timeout-proc expect-timeout))
       #'(let* ((port expect-port)
                (char-proc expect-char-proc)
                (pass-char? expect-pass-char?)
                (eof-proc expect-eof-proc)
                (timeout-proc expect-timeout-proc)
                (timeout expect-timeout)
                (timeout
                 (if timeout
                     (let ((secs-usecs (gettimeofday)))
                       (+ (car secs-usecs)
                          timeout
                          (/ (cdr secs-usecs)
                             ;; one million.
                             1000000)))
                     #f))
                predicate-bindings ...)
           (let loop ((content ""))
             (if (and timeout (not (expect-select port timeout)))
                 (and timeout-proc (timeout-proc content))
                 (let* ((char (read-char port))
                        (eof? (eof-object? char))
                        (next-content
                         (or (and (not eof?)
                                  (string-append
                                   content (string char)))
                             content)))
                   (when (and (not pass-char?) char-proc)
                     (char-proc char))
                   (cond
                    ((predicate-binding next-content
                      (or (and (not pass-char?) eof?)
                          (and (not eof?) char)))
                     body ...)
                    ...
                    (eof? (and eof-proc (eof-proc content)))
                    (else (loop next-content)))))))))))

(define (bind-locally stx sym)
  (any
   (lambda (id)
     (and (eqv? sym (syntax->datum id))
          (datum->syntax stx sym)))
   (syntax-locally-bound-identifiers stx)))

(define-syntax expect
  (lambda (stx)
    (syntax-case stx ()
      ((expect clause clauses ...)
       (with-syntax
           ((expect-port
             (or (bind-locally #'expect 'expect-port)
                 (syntax (current-input-port))))
            (expect-char-proc
             (or (bind-locally #'expect 'expect-char-proc)
                 (syntax #f)))
            (expect-eof-proc
             (or (bind-locally #'expect 'expect-eof-proc)
                 (syntax #f)))
            (expect-timeout
             (or (bind-locally #'expect 'expect-timeout)
                 (syntax #f)))
            (expect-timeout-proc
             (or (bind-locally #'expect 'expect-timeout-proc)
                 (syntax #f)))
            (expect-pass-char?
             (or (bind-locally #'expect 'expect-pass-char?)
                 (syntax #f))))
         #'(expect-with-bindings
            () () (clause clauses ...)
            (expect-port expect-eof-proc
             expect-char-proc expect-pass-char?
             expect-timeout-proc expect-timeout)))))))

(define (expect-regexec rx s eof? exec-flags)
  ;; if expect-strings-exec-flags contains regexp/noteol,
  ;; remove it for the eof test.
  (let* ((flags (if (and eof? (logand exec-flags regexp/noteol))
		    (logxor exec-flags regexp/noteol)
		    exec-flags))
	 (match (regexp-exec rx s 0 flags)))
    (if match
	(do ((i (- (rx:match:count match) 1) (- i 1))
	     (result '() (cons (rx:match:substring match i) result)))
	    ((< i 0) result))
	#f)))

(define-syntax expect-strings
  (lambda (stx)
    (syntax-case stx ()
      ((expect-strings (pattern body more-body ...) ...)
       (with-syntax ((expect-strings-compile-flags
                      (or (bind-locally #'expect-strings 'expect-strings-compile-flags)
                          (syntax regexp/newline)))
                     (expect-strings-exec-flags
                      (or (bind-locally #'expect-strings 'expect-strings-exec-flags)
                          (syntax regexp/noteol)))
                     (expect-pass-char? #f)
                     (expect (datum->syntax #'expect-strings 'expect)))
         #'(let* ((compile-flags expect-strings-compile-flags)
                  (exec-flags expect-strings-exec-flags))
             (expect
              ((let ((rx (make-regexp pattern compile-flags)))
                 (lambda (content eof?)
                   (expect-regexec rx content eof? exec-flags)))
               body more-body ...) ...)))))))

(define-syntax interact
  (lambda (stx)
    (syntax-case stx ()
      ((interact)
       (with-syntax ((expect-timeout (datum->syntax #'interact 'expect-timeout))
                     (expect (datum->syntax #'interact 'expect)))
         #'(let ((interaction
                  (call-with-new-thread
                   (lambda ()
                     (let loop ((line (readline)))
                       (cond
                        ((equal? line "continue!")
                         (newline port))
                        (else
                         (display line port)
                         (newline port)
                         (loop (readline))))))))
                 ;; always disable timeouts
                 (expect-timeout #f))
             (let loop ()
               (expect
                ((const #t)
                 (cond
                  ((thread-exited? interaction)
                   (format #t "\nCONTINUING...\n"))
                  (else (loop))))))))))))

(set! test-log-to-file #f)

(let ((id (let ((a 3) (b 7)) #'x))
      (z 31))
  (test-begin "bind-locally")
  (test-assert "found in context"
    (bound-identifier=?
     (datum->syntax id 'a)
     (bind-locally id 'a)))
  (test-assert "found in context"
    (bound-identifier=?
     (datum->syntax id 'b)
     (bind-locally id 'b)))
  (test-assert "missing in context"
    (not (bind-locally id 'z)))
  (test-end "bind-locally"))
