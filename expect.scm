(define-module (common expect)
  #:export (expect-chars))

(add-to-load-path
 (dirname (dirname (current-filename))))

(use-modules
 ((common utils) #:select
  (syntax-capture)))

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
        (expect-port expect-eof-proc
         expect-timeout-proc expect-timeout))
       #'(let* ((input-port expect-port)
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
                        (char (and (not (eof-object? char)) char))
                        (next-content
                         (or (and char
                                  (string-append
                                   content (string char)))
                             content)))
                   (cond
                    ((matcher-binding next-content char) body ...)
                    ...
                    ((not char) (and eof-proc (eof-proc content)))
                    (else (loop next-content)))))))))))

(define-syntax expect-chars
  (lambda (stx)
    (syntax-case stx ()
      ((expect-chars clause clauses ...)
       (with-syntax
           ((expect-port
             (or (syntax-capture #'expect-chars 'expect-port)
                 (syntax (current-input-port))))
            (expect-eof-proc
             (or (syntax-capture #'expect-chars 'expect-eof-proc)
                 (syntax #f)))
            (expect-timeout
             (or (syntax-capture #'expect-chars 'expect-timeout)
                 (syntax #f)))
            (expect-timeout-proc
             (or (syntax-capture #'expect-chars 'expect-timeout-proc)
                 (syntax #f))))
         #'(expect-with-bindings
            () () (clause clauses ...)
            (expect-port expect-eof-proc
             expect-timeout-proc expect-timeout)))))))

