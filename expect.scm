(define-module (common expect)
  #:export
  (expect-timeout expect-timeout-proc
   expect-char-proc expect-eof-proc
   expect-strings-compile-flags
   expect-strings-exec-flags
   expect-port))

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
        (procedure-bindings ...)
        (clauses-with-bindings ...)
        ((matcher => consumer) more-clauses ...))
       #'(expect-with-bindings
          (procedure-bindings
           ...
           (matcher-binding matcher)
           (consumer-binding consumer)
           (consumer-binding (lambda (result) (apply consumer-binding result))))
          (clauses-with-bindings
           ...
           (matcher-binding => consumer-binding))
          (more-clauses ...)))

      ((expect-with-bindings
        (procedure-bindings ...)
        (clauses-with-bindings ...)
        ((matcher body ...) more-clauses ...))
       #'(expect-with-bindings
          (procedure-bindings
           ...
           (matcher-binding matcher))
          (clauses-with-bindings
           ...
           (matcher-binding body ...))
          (more-clauses ...)))

      ((expect-with-bindings
        (procedure-bindings ...)
        ((matcher-binding body ...) ...)
        ())
       (with-syntax ((expect-port (datum->syntax stx 'expect-port))
                     (expect-timeout (datum->syntax stx 'expect-timeout))
                     (expect-timeout-proc (datum->syntax stx 'expect-timeout-proc))
                     (expect-char-proc (datum->syntax stx 'expect-char-proc))
                     (expect-eof-proc (datum->syntax stx 'expect-eof-proc)))
         #'(let ((input-port (or expect-port (current-input-port)))
                 (timeout
                  (if expect-timeout
                      (let ((secs-usecs (gettimeofday)))
                        (+ (car secs-usecs)
                           expect-timeout
                           (/ (cdr secs-usecs)
                              ;; one million.
                              1000000)))
                      #f))
                 procedure-bindings
                 ...
                 (content ""))
             (let loop ()
               (if (and expect-timeout (not (expect-select input-port timeout)))
                   (and expect-timeout-proc (expect-timeout-proc content))
                   (let* ((char (read-char input-port))
                          (eof? (eof-object? char)))
                     (when expect-char-proc
                       (expect-char-proc char))
                     (when (not eof?)
                       (set! content
                        (string-append content (string char))))
                     (cond
                      ((matcher-binding content eof?) body ...)
                      ...
                      (eof? (and expect-eof-proc (expect-eof-proc content)))
                      (else (loop))))))))))))

(define-syntax expect
  (lambda (stx)
    (syntax-case stx ()
      ((expect clause clauses ...)
       #'(expect-with-bindings () () (clause clauses ...))))))
