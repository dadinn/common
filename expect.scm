(define-module (common expect)
  #:export (expect expect-chars expect-strings interact))

(add-to-load-path
 (dirname (dirname (current-filename))))

(use-modules
 ((common utils) #:select
  (syntax-capture))
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
         #'(interact expect-port)))
      ((interact interact-port)
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
                  (output-port interact-port)
                  (interact-thread
                   (call-with-new-thread
                    (lambda ()
                      (let interact-loop ((line (readline)))
                        (unless (string-ci=? line ",quit")
                          (display line output-port)
                          (newline output-port)
                          (interact-loop (readline))))
                      (newline output-port))))
                  (char-proc expect-char-proc)
                  (interact-matcher
                   (lambda (content char)
                     (when (and char-proc char)
                       (char-proc char))
                     #t)))
             (let interact-loop ()
               (expect-with-bindings
                () ()
                ((interact-matcher
                  (unless (thread-exited? interact-thread)
                    (interact-loop))))
                (input-port expect-eof-proc
                 expect-timeout-proc expect-timeout)))))))))

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

   (display "
EXPANSION: expect-chars macro

Verify that default current-input-port is
used instead of missing expect-port binding,
expect-char-proc binding is ignored, while
all other parameters are captured/rebound.

")
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
          => (lambda (x y z) (+ x y z)))))))

   (display "
EXPANSION: bacwards-compatible expect macro

Verify that default current-input-port is
used instead of missing expect-port binding,
while other parameters are captured/rebound,
including expect-char-proc.

")
   (pretty-print
    (tree-il->scheme
     (macroexpand
      #'(expect
         ((lambda (s eof?)
            (rx:string-match "[a-zA-Z]*[0-9]+" s))
          (display "FOO1")
          (display "BAR1"))
         ((lambda (s eof?)
            (= 42 foobar))
          (display "FOO2")
          (display "BAR2"))
         ((lambda (s eof?)
            (and (not eof?) (list 1 2 3)))
          => (lambda (x y z) (+ x y z))))))))

 (let ((expect-strings-exec-flags #f)
       (expect-char-proc display))

   (display "EXPANSION: expect-strings macro

Verify that expect-strings-exec-flags and
expect-char-proc get captured/rebound internally
while default regexp/newline is used instead
of missing expect-strings-compile-flags

")
   (pretty-print
    (tree-il->scheme
     (macroexpand
      #'(expect-strings
         ("Welcome to GRUB[!?]+"
          (sleep 3)
          (display "booting GRUB... it seems!"))))))

   (display "
EXPANSION: expect-strings macro with consumer procedure syntax

Verify that both lambda expressions for the string pattern matcher,
and the consumer procuder gets evaluated and bound only once.

")
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

 (let ((expect-timeout 1000))
   (display "
EXPANSION: interact using default STDIN/STDOUT ports

Verify that without expect-port binding in context,
current-input-port and current-output-port are used.

")
   (pretty-print
    (tree-il->scheme
     (macroexpand
      #'(interact)))))

 (let ((expect-port #t)
       (expect-char-proc display)
       (expect-timeout 1000)
       (null-port (%make-void-port OPEN_WRITE)))
   (display "
EXPANSION: interact using expect-port for both input and output

Verify that expect-timeout is overridden,
and expect-port is properly captured/rebound as input-port
and expect-char-proc is properly captured/rebound too.

")
   (pretty-print
    (tree-il->scheme
     (macroexpand
      #'(interact))))

   (display "
EXPANSION: interact using expect-port only for input

Verify same as before, but null-port is captured/rebound as output-port.

")
   (pretty-print
    (tree-il->scheme
     (macroexpand
      #'(interact null-port)))))

 (let ((expect-char-proc
        (lambda (c) (format #t "Read char: ~A\n" c))))

   (display "TESTING: expect-chars macro with procedural matching!\n\n")
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

   (display "\nTESTING: backwards-compatible expect macro with procedural matching!\n\n")
   (call-with-input-string
       "Some stuff here...
MATCHING_STUFF_2023!!!"
     (lambda (expect-port)
       (expect
        ((lambda (s eof?)
           ;; expect-eof-flag macro is backwards compatible with original expect macro,
           ;; captures expect-char-proc binding, and calls it with each non-EOF character read.
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
