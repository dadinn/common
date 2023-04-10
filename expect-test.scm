
(add-to-load-path
 (dirname (dirname (current-filename))))

(use-modules
 ((common expect))
 ((common utils) #:select
  (comment error*))
 ((srfi srfi-64))
 ((ice-9 threads) #:select
  (call-with-new-thread
   thread-exited?))
 ((ice-9 regex))
 (ice-9 popen))

(set! test-log-to-file #f)

(test-begin "expect")

(test-equal "expect-chars macro ignores expect-char-proc bindings and receives read char directly"
  "MATCHING_STUFF_2023!"
  (let ((expect-char-proc
         (lambda (c)
           ;; expect-chars macro ignores expect-char-proc bindings
           (error "expect-char-proc must not be captured, but ignored!")))
        (counter 0))
    (match:substring
     (call-with-input-string
         "Some stuff here...
MATCHING_STUFF_2023!!!"
       (lambda (expect-port)
         (expect-chars
          ((begin
             (set! counter (+ 1 counter))
             (lambda (content char)
               (when (not (< 0 counter 2))
                 (error* "matcher expression must be only evaluated once!"))
               (string-match "MATCHING_STUFF_[0-9]+[^0-9]" content)))))))
     0)))

(test-equal "expect macro captures and uses expect-char-proc bindings"
  "Some stuff here...
MATCHING_STUFF_2023!
MATCHED!!!"
  (call-with-output-string
    (lambda (output-port)
      (let ((expect-char-proc
             ;; expect macro should capture and use expect-char-proc binding
             (lambda (c) (display c output-port)))
            (counter 0))
        (call-with-input-string
            "Some stuff here...
MATCHING_STUFF_2023!!!"
          (lambda (expect-port)
            (expect
             ((begin
                (set! counter (+ 1 counter))
                (lambda (content eof?)
                  (when (not (< 0 counter 2))
                    (error* "matcher expression must be only evaluated once!"))
                  (string-match "MATCHING_STUFF_[0-9]+[^0-9]" content)))
              (display "\nMATCHED!!!" output-port)))))))))

(test-equal "expect macro captures and uses expect-eof-proc bindings"
  "UNMATCHED: SOME_STUFF"
  (let ((expect-eof-proc
         (lambda (content) (string-append "UNMATCHED: " content))))
    (call-with-input-string "SOME_STUFF"
      (lambda (expect-port)
        (expect ((const #f)))))))

(test-eqv "expect-strings macro works with plain regex matcher and body"
  37
  (let ((num 43))
    (call-with-input-string
        "Some stuff here...
MATCHING_STUFF_2023!!!"
      (lambda (expect-port)
        (expect-strings
         ("\nMATCHING_STUFF_[0-9]+[^0-9]"
          (set! num (+ num 13))
          (set! num (- num 19))
          num))))))

(test-equal "expect-strings macro works with match result consumer"
  '("MATCHING_STUFF_2023!" "STUFF" "2023")
  (call-with-input-string
      "Some stuff here...
MATCHING_STUFF_2023!!!"
    (lambda (expect-port)
      (expect-strings
       ("MATCHING_([^_]+)_([0-9]+)[^0-9]" =>
        (lambda (all id num)
          (list all id num)))))))

(test-equal "expect-strings handles timeout with expect-timout-proc"
  "DO IT!!!
"
   (let ((expect-timeout 1)
         (expect-timeout-proc
          (lambda (content)
            content))
         (expect-eof-proc
          (lambda (content)
            (error "MUST NOT BE CALLED!!!")))
         (expect-port (open-input-pipe "echo DO IT!!!; sleep 2")))
     (dynamic-wind
       (const #t)
       (lambda ()
         (expect-strings
          ("WAITING FOREVER..."
           (error "NEVER SHOULD HAVE ARRIVED!!!"))))
       (lambda ()
         (close-pipe expect-port)))))

(test-equal "interact captures and uses expect-eof-proc"
  "INTER...
...ACTION
"
  (let* ((expect-eof-proc
          (lambda (content)
            content))
         (expect-timeout-proc
          (lambda (content)
            (error "MUST NOT BE CALLED!!!")))
         ;; mute printing chars from expect-port to STDOUT
         (expect-char-proc (const #t))
         (expect-port
          (open-input-pipe "echo INTER...; sleep 1; echo ...ACTION"))
         (interact-output-port (%make-void-port OPEN_WRITE)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (interact interact-output-port))
      (lambda ()
        (close-port interact-output-port)
        (close-pipe expect-port)))))

(test-equal "interact ignores timeout and exits on ,return command"
  "INTERACT...
...CONTINUE!"
  (call-with-output-string
    (lambda (output-port)
      (let* ((expect-char-proc
              (lambda (c) (display c output-port)))
             (expect-eof-proc
              (lambda (content)
                (error "MUST NOT BE CALLED!!!")))
             (expect-timeout-proc
              (lambda (content)
                (error "MUST NOT BE CALLED!!!")))
             (expect-timeout 1)
             (expect-port
              (open-input-pipe "echo INTERACT...; sleep 2; echo ...CONTINUE!"))
             (interact-input-port
              (open-input-pipe "sleep 1; echo ,return"))
             (interact-output-port
              (%make-void-port OPEN_WRITE)))
        (dynamic-wind
          (const #t)
          (lambda ()
            (with-input-from-port interact-input-port
              (lambda ()
                (interact interact-output-port)))
            (expect-strings
             (".+CONTINUE!" 42)))
          (lambda ()
            (close-port interact-output-port)
            (close-pipe interact-input-port)
            (close-pipe expect-port)))))))

(test-end "expect")
