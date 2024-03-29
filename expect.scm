(define-module (common expect)
  #:export (expect expect-chars expect-strings interact))

(use-modules
 ((common utils) #:select
  (syntax-capture))
 ((ice-9 threads) #:select
  (cancel-thread thread-exited?
   call-with-new-thread))
 ((ice-9 rdelim) #:select (read-line))
 ((ice-9 regex) #:prefix rx:))

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
                    ((matcher-binding next-content char)
                     body ...)
                    ...
                    ((not char)
                     (and eof-proc (eof-proc content)))
                    (else
                     (loop next-content)))))))))))

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

(define-syntax expect
  (lambda (stx)
    (syntax-case stx ()
      ((expect (matcher-with-eof-flag body ...) ...)
       (with-syntax
           ((expect-char-proc
             (or (syntax-capture #'expect 'expect-char-proc)
                 (syntax #f)))
            (expect-port
             (or (syntax-capture #'expect 'expect-port)
                 (syntax (current-input-port))))
            (expect-eof-proc
             (or (syntax-capture #'expect 'expect-eof-proc)
                 (syntax #f)))
            (expect-timeout
             (or (syntax-capture #'expect 'expect-timeout)
                 (syntax #f)))
            (expect-timeout-proc
             (or (syntax-capture #'expect 'expect-timeout-proc)
                 (syntax #f))))
         #'(let* ((char-proc expect-char-proc))
             (expect-with-bindings
              () ()
              (((let ((matcher-inner-binding matcher-with-eof-flag))
                   (lambda (content char)
                     (when (and char-proc char)
                       (char-proc char))
                     (matcher-inner-binding content (not char))))
                body ...) ...)
            (expect-port expect-eof-proc
             expect-timeout-proc expect-timeout))))))))

(define (expect-regexec rx s eof? exec-flags)
  ;; if expect-strings-exec-flags contains regexp/noteol,
  ;; remove it for the eof test.
  (let* ((flags (if (and eof? (logand exec-flags regexp/noteol))
                    (logxor exec-flags regexp/noteol)
                    exec-flags))
         (matches (regexp-exec rx s 0 flags)))
    (if matches
        (do ((i (- (rx:match:count matches) 1) (- i 1))
             (result '() (cons (rx:match:substring matches i) result)))
            ((< i 0) result))
        #f)))

(define-syntax expect-strings
  (lambda (stx)
    (syntax-case stx ()
      ((expect-strings (pattern body more-body ...) ...)
       (with-syntax
           ((expect-strings-compile-flags
             (or (syntax-capture #'expect-strings 'expect-strings-compile-flags)
                 (syntax regexp/newline)))
            (expect-strings-exec-flags
             (or (syntax-capture #'expect-strings 'expect-strings-exec-flags)
                 (syntax regexp/noteol)))
            (expect-char-proc
             (or (syntax-capture #'expect-strings 'expect-char-proc)
                 (syntax #f)))
            (expect-port
             (or (syntax-capture #'expect-strings 'expect-port)
                 (syntax (current-input-port))))
            (expect-eof-proc
             (or (syntax-capture #'expect-strings 'expect-eof-proc)
                 (syntax #f)))
            (expect-timeout
             (or (syntax-capture #'expect-strings 'expect-timeout)
                 (syntax #f)))
            (expect-timeout-proc
             (or (syntax-capture #'expect-strings 'expect-timeout-proc)
                 (syntax #f))))
         #'(let* ((compile-flags expect-strings-compile-flags)
                  (exec-flags expect-strings-exec-flags)
                  (char-proc expect-char-proc))
             (expect-with-bindings
              () ()
              (((let ((rx (make-regexp pattern compile-flags)))
                  (lambda (content char)
                    (when (and char-proc char)
                      (char-proc char))
                    (expect-regexec rx content (not char) exec-flags)))
                body more-body ...) ...)
              (expect-port expect-eof-proc
               expect-timeout-proc expect-timeout))))))))

(define-syntax interact
  (lambda (stx)
    (syntax-case stx ()
      ((interact)
       (with-syntax
           ((expect-port
             (or (syntax-capture #'interact 'expect-port)
                 (syntax (current-output-port)))))
         #'(interact expect-port expect-port)))
      ((interact interact-output-port)
       (with-syntax
           ((expect-port
             (or (syntax-capture #'interact 'expect-port)
                 (syntax (current-output-port)))))
         #'(interact expect-port interact-output-port)))
      ((interact interact-input-port interact-output-port)
       (with-syntax
           ((expect-port
             (or (syntax-capture #'interact 'expect-port)
                 (syntax (current-input-port))))
            (expect-char-proc
             (or (syntax-capture #'interact 'expect-char-proc)
                 (syntax display)))
            (expect-eof-proc
             (or (syntax-capture #'interact 'expect-eof-proc)
                 (syntax #f)))
            ;; always disable timeouts
            (expect-timeout (syntax #f))
            (expect-timeout-proc (syntax #f)))
         #'(let* ((input-port expect-port)
                  (output-port interact-output-port)
                  (char-proc expect-char-proc)
                  (interact-thread
                   (call-with-new-thread
                    (lambda ()
                      (let interact-loop ((line (read-line)))
                        (unless
                            (or (eof-object? line)
                                (string-ci=? line ",return"))
                          (display line output-port)
                          (newline output-port)
                          (force-output output-port)
                          (interact-loop (read-line))))
                      (newline output-port)))))
             (dynamic-wind
               (const #t)
               (lambda ()
                 (expect-with-bindings
                  () ()
                  (((lambda (content char)
                      (when (and char-proc char)
                        (char-proc char))
                      (thread-exited? interact-thread))))
                  (input-port expect-eof-proc
                   expect-timeout-proc expect-timeout)))
               (lambda ()
                 (cancel-thread interact-thread)))))))))
