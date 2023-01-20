(define-module (common expect)
  #:export
  (expect-timeout expect-timeout-proc
   expect-char-proc expect-eof-proc
   expect-strings-compile-flags
   expect-strings-exec-flags
   expect-port))

(use-modules
 (system syntax)
 ((ice-9 popen) #:prefix popen:)
 ((ice-9 regex) #:prefix rx:)
 ((ice-9 format) #:select (format))
 ((srfi srfi-1) #:select (any))
 (srfi srfi-64))

(define expect-port (current-input-port))
(define expect-timeout #f)
(define expect-timeout-proc #f)
(define expect-eof-proc #f)
(define expect-char-proc #f)
(define expect-strings-compile-flags regexp/newline)
(define expect-strings-exec-flags regexp/noteol)

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
        ((predicate => procedure) more-clauses ...))
       #'(expect-with-bindings
          (predicate-bindings
           ...
           (predicate-binding predicate))
          (clauses-with-bindings
           ...
           (predicate-binding => (lambda (result) (apply procedure result))))
          (more-clauses ...)))

      ((expect-with-bindings
        (predicate-bindings ...)
        (clauses-with-bindings ...)
        ((predicate body ...) more-clauses ...))
       #'(expect-with-bindings
          (predicate-bindings
           ...
           (predicate-binding predicate))
          (clauses-with-bindings
           ...
           (predicate-binding body ...))
          (more-clauses ...)))

      ((expect-with-bindings
        (predicate-bindings ...)
        ((predicate-binding body ...) ...)
        ())
       (with-syntax ((expect-port (datum->syntax stx 'expect-port))
                     (expect-timeout (datum->syntax stx 'expect-timeout))
                     (expect-timeout-proc (datum->syntax stx 'expect-timeout-proc))
                     (expect-char-proc (datum->syntax stx 'expect-char-proc))
                     (expect-eof-proc (datum->syntax stx 'expect-eof-proc)))
         #'(let ((port (or expect-port (current-input-port)))
                 (timeout
                  (if expect-timeout
                      (let ((secs-usecs (gettimeofday)))
                        (+ (car secs-usecs)
                           expect-timeout
                           (/ (cdr secs-usecs)
                              ;; one million.
                              1000000)))
                      #f))
                 predicate-bindings
                 ...
                 (content ""))
             (let loop ()
               (if (and expect-timeout (not (expect-select port timeout)))
                   (and expect-timeout-proc (expect-timeout-proc content))
                   (let* ((char (read-char port))
                          (eof? (eof-object? char)))
                     (when expect-char-proc
                       (expect-char-proc char))
                     (when (not eof?)
                       (set! content
                        (string-append content (string char))))
                     (cond
                      ((predicate-binding content eof?) body ...)
                      ...
                      (eof? (and expect-eof-proc (expect-eof-proc content)))
                      (else (loop))))))))))))

(define-syntax expect
  (lambda (stx)
    (syntax-case stx ()
      ((expect clause clauses ...)
       #'(expect-with-bindings () () (clause clauses ...))))))
