(define-module (common expect)
  #:export (expect-chars))

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
        (#:context context contexts ...)
        (procedure-bindings ...)
        (clauses-with-bindings ...)
        ((matcher => consumer) more-clauses ...))
       #'(expect-with-bindings
          (#:context context contexts ...)
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
        (#:context context contexts ...)
        (procedure-bindings ...)
        (clauses-with-bindings ...)
        ((matcher body ...) more-clauses ...))
       #'(expect-with-bindings
          (#:context context contexts ...)
          (procedure-bindings
           ...
           (matcher-binding matcher))
          (clauses-with-bindings
           ...
           (matcher-binding body ...))
          (more-clauses ...)))

      ((expect-with-bindings
        (#:context context contexts ...)
        (procedure-bindings ...)
        ((matcher-binding body ...) ...)
        ()) ; no more clauses
       (with-syntax
           ((expect-port
             (or (bind-locally 'expect-port #'(context contexts ...))
                 (syntax (current-input-port))))
            (expect-eof-proc
             (or (bind-locally 'expect-eof-proc #'(context contexts ...))
                 (syntax #f)))
            (expect-timeout-proc
             (or (bind-locally 'expect-timeout-proc #'(context contexts ...))
                 (syntax #f)))
            (expect-timeout
             (or (bind-locally 'expect-timeout #'(context contexts ...))
                 (syntax #f))))
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
                    (else (loop next-content))))))))))))

(define-syntax expect-chars
  (lambda (stx)
    (syntax-case stx ()
      ((expect-chars
        (#:context context ...)
        clause more-clauses ...)
       #'(expect-with-bindings
          (#:context expect-chars context ...)
          () () (clause more-clauses ...)))
      ((expect-chars
        clause more-clauses ...)
       #'(expect-chars
          (#:context)
          clause more-clauses ...)))))

