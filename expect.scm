(define-module (common expect)
  #:export (expect expect-chars expect-strings interact))

(add-to-load-path
 (dirname (dirname (current-filename))))

(use-modules
 ((common utils) #:select
  (bind-locally comment))
 ((ice-9 threads) #:select
  (call-with-new-thread thread-exited?))
 ((ice-9 readline) #:select (readline))
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

(define-syntax expect
  (lambda (stx)
    (syntax-case stx ()
      ((expect (matcher-with-eof-flag body ...) ...)
       (with-syntax
           ((expect-char-proc
             (or (bind-locally 'expect-char-proc #'(expect))
                 (syntax #f))))
         #'(let ((char-proc expect-char-proc))
             (expect-chars
              (#:context expect)
              ((let ((matcher-inner-binding matcher-with-eof-flag))
                 (lambda (content char)
                   (when (and char-proc char)
                     (char-proc char))
                   (matcher-inner-binding content (not char))))
               body ...) ...)))))))

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
             (or (bind-locally 'expect-strings-compile-flags #'(expect-strings))
                 (syntax regexp/newline)))
            (expect-strings-exec-flags
             (or (bind-locally 'expect-strings-exec-flags #'(expect-strings))
                 (syntax regexp/noteol)))
            (expect-char-proc
             (or (bind-locally 'expect-char-proc #'(expect-strings))
                 (syntax #f))))
         #'(let* ((compile-flags expect-strings-compile-flags)
                  (exec-flags expect-strings-exec-flags)
                  (char-proc expect-char-proc))
             (expect-chars
              (#:context expect-strings)
              ((let ((rx (make-regexp pattern compile-flags)))
                 (lambda (content char)
                   (when (and char-proc char)
                     (char-proc char))
                   (expect-regexec rx content (not char) exec-flags)))
               body more-body ...) ...)))))))

(define-syntax interact
  (lambda (stx)
    (syntax-case stx ()
      ((interact)
       (with-syntax
           ((expect-port
             (or (bind-locally 'expect-port #'(interact))
                 (syntax (current-output-port)))))
         #'(interact expect-port)))
      ((interact interact-port)
       (with-syntax
           ((expect (datum->syntax #'interact 'expect))
            (expect-timeout (datum->syntax #'interact 'expect-timeout)))
         #'(let* ((interact-thread
                   (call-with-new-thread
                    (lambda ()
                      (let interact-loop ((line (readline)))
                        (unless (string-ci=? line ",quit")
                          (display line interact-port)
                          (newline interact-port)
                          (interact-loop (readline))))
                      (newline interact-port))))
                  ;; always disable timeouts
                  (expect-timeout #f))
             (let interact-loop ()
               (expect
                ((const #t)
                 (unless (thread-exited? interact-thread)
                   (interact-loop)))))))))))

(comment
 ;; manual tests and macro expansions

 (use-modules
  (language tree-il)
  (ice-9 pretty-print))

 (let ((expect-eof-proc #t)
       (expect-char-proc display)
       (expect-timeout-proc #t)
       (expect-timeout #t)
       (foobar 13))

   ;; verify that default current-input-port is
   ;; used instead of missing expect-port binding,
   ;; while other parameters are captured/rebound,
   ;; except expect-char-proc, which is ignored.
   (display "\nEXPANSION: expect-chars macro\n\n")
   (pretty-print
    (tree-il->scheme
     (macroexpand
      #'(expect-chars
         ((lambda (s c)
            (rx:string-match "[a-zA-Z]*[0-9]+" s))
          (display "FOO1")
          (display "BAR1"))
         ((lambda (s c)
            (= 42 foobar))
          (display "FOO2")
          (display "BAR2"))
         ((lambda (s c)
            (list 1 2 3))
          => (lambda (x y z) (+ x y z))))))))

 (let ((expect-strings-exec-flags #f)
       (expect-char-proc display))

   ;; verify that expect-strings-exec-flags
   ;; and expect-char-proc get captured/rebound internally
   (display "\nEXPANSION: expact-strings macro\n\n")
   (pretty-print
    (tree-il->scheme
     (macroexpand
      #'(expect-strings
         ("Welcome to GRUB[!?]+"
          (sleep 3)
          (display "booting GRUB... it seems!"))))))

   (display "\nEXPANSION: expact-strings macro with consumer procedure syntax\n\n")
   (pretty-print
    (tree-il->scheme
     (macroexpand
      #'(expect-strings
         ("\nMATCHING_([^_]+)_([0-9]+)[^0-9]" =>
          (lambda (all id num)
            (format #t "
MATCHED!!!

WHOLE MATCH: ~A

SUB MATCH 1: ~A
SUB MATCH 2: ~A
" all id num))))))))

 (let ((expect-char-proc
        (lambda (c) (format #t "Read char: ~A\n" c))))

   ;; run expect-chars with procedural matching
   (display "\nTESTING: expect-chars macro with procedural matching!\n\n")
   (call-with-input-string
       "Some stuff here...
MATCHING_STUFF_2023!!!"
     (lambda (expect-port)
       (expect-chars
        ((lambda (s c)
           ;; displaying read characters is the responsibility of the matcher,
           ;; as expect-char-proc parameter is not captured by expect-chars macro
           (and c (expect-char-proc c))
           (rx:string-match "\nMATCHING_STUFF_[0-9]+[^0-9]" s))
         (display "\nMATCHED!!!\n")))))

   ;; run expect matching with regex pattern
   (display "\nTESTING: expect-strings macro with regex pattern matching!\n\n")
   (call-with-input-string
       "Some stuff here...
MATCHING_STUFF_2023!!!"
     (lambda (expect-port)
       (expect-strings
        ("\nMATCHING_STUFF_[0-9]+[^0-9]"
         (display "\nMATCHED!!!\n")))))
   (display "\nTESTING: expect-strings macro with regex pattern matching and post-processing of results!\n\n")
   (call-with-input-string
       "Some stuff here...
MATCHING_STUFF_2023!!!"
     (lambda (expect-port)
       (expect-strings
        ("\nMATCHING_([^_]+)_([0-9]+)[^0-9]" =>
         (lambda (all id num)
           (format #t "
MATCHED!!!

WHOLE MATCH: ~A

SUB MATCH 1: ~A
SUB MATCH 2: ~A
" all id num)))))))

 ) ; END OF TESTS
