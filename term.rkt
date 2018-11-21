#lang racket/base
(require racket/match
         racket/port
         racket/format
         racket/system)

(struct choice (tag disp exe))

(define (string-but-last s)
  (substring s 0 (sub1 (string-length s))))

(define (fzf l default)
  (define-values
    (sp p-output p-input p-error)
    (subprocess #f (if l #f (current-input-port)) (current-error-port)
                (find-executable-path "fzf")))
  (when l
    (for ([c (in-list l)]
          [i (in-naturals)])
      (fprintf p-input "~a ~a ~a\n" (choice-tag c) i (choice-disp c)))
    (flush-output p-input))
  (subprocess-wait sp)
  (define outs (port->string p-output))
  (cond
    [l
     (match-define (regexp #rx"^[^ ]+ ([0-9]+) " (list _ which-s))
       outs)
     ((choice-exe (list-ref l (string->number which-s))))]
    [else
     (default (string-but-last outs))]))

(define (term-main #:options [opts (default-opts)]
                   #:default [default default-select])
  (fzf opts default))

(define (default-opts)
  (list (choice 'open "Open a file"
                (λ () (fzf #f (λ (s) (exec (~a "open '" s "'"))))))
        (choice 'edit "Edit a file"
                (λ () (fzf #f (λ (s) (exec (~a "${EDITOR} '" s "'"))))))))

(define (default-select s)
  (displayln s))

(define (exec s)
  (local-require ffi/unsafe)
  (define default-lib (ffi-lib #f))
  (define execv
    (get-ffi-obj "execv" default-lib
                 (_fun #:save-errno 'posix
                       _path (_vector i _string/eof)
                       -> _int)))
  (execv "/bin/sh" (vector "/bin/sh" "-c" s eof))
  (error 'exec "Failed to exec: ~v" (saved-errno)))

(provide term-main
         default-opts
         default-select)
