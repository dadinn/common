(define-module (common expect)
  #:export (expect))

(add-to-load-path
 (dirname (dirname (current-filename))))

(use-modules
 ((common utils)
  #:select (bind-locally)))

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
        ((matcher => consumer) more-clauses ...)
        other-params)
       #'(expect-with-bindings
          (procedure-bindings
           ...
           (matcher-binding matcher)
           (consumer-binding consumer)
           (consumer-binding (lambda (result) (apply consumer-binding result))))
          (clauses-with-bindings
           ...
           (matcher-binding => consumer-binding))
          (more-clauses ...)
          other-params))

      ((expect-with-bindings
        (procedure-bindings ...)
        (clauses-with-bindings ...)
        ((matcher body ...) more-clauses ...)
        other-params)
       #'(expect-with-bindings
          (procedure-bindings
           ...
           (matcher-binding matcher))
          (clauses-with-bindings
           ...
           (matcher-binding body ...))
          (more-clauses ...)
          other-params))

      ((expect-with-bindings
        (procedure-bindings ...)
        ((matcher-binding body ...) ...)
        () ; no more clauses
        ;; other parameters
        (expect-port expect-char-proc expect-eof-proc
         expect-timeout-proc expect-timeout))
       #'(let* ((input-port expect-port)
                (char-proc expect-char-proc)
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
                procedure-bindings ...)
           (let loop ((content ""))
             (if (and timeout (not (expect-select input-port timeout)))
                 (and timeout-proc (timeout-proc content))
                 (let* ((char (read-char input-port))
                        (eof? (eof-object? char))
                        (next-content
                         (or (and (not eof?)
                                  (string-append
                                   content (string char)))
                             content)))
                   (when char-proc
                     (char-proc char))
                   (cond
                    ((matcher-binding next-content eof?) body ...)
                    ...
                    (eof? (and eof-proc (eof-proc content)))
                    (else (loop next-content)))))))))))

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
                 (syntax #f))))
         #'(expect-with-bindings
            () () (clause clauses ...)
            (expect-port expect-char-proc expect-eof-proc
             expect-timeout-proc expect-timeout)))))))

