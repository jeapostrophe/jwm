#lang racket/base
(require racket/match
         racket/port
         racket/format
         racket/system)

(struct choice (disp exe))

(define (string-but-last s)
  (substring s 0 (sub1 (string-length s))))

(define (fzf l select)
  (define-values
    (sp p-output p-input p-error)
    (subprocess #f (if l #f (current-input-port)) (current-error-port)
                (find-executable-path "fzf")
                "-d" " "
                (if l
                  "--with-nth=2.."
                  "--with-nth=..")))
  (when l
    (for ([c (in-list l)]
          [i (in-naturals)])
      (fprintf p-input "~a ~a\n" i (choice-disp c)))
    (close-output-port p-input))
  (subprocess-wait sp)
  (define outs (port->string p-output))
  (cond
    [(string=? outs "")
     (exit 1)]
    [l
     (match-define (regexp #rx"^([0-9]+) " (list _ which-s))
       outs)
     ((choice-exe (list-ref l (string->number which-s))))]
    [else
     (select (string-but-last outs))]))

(define (term-main #:options [opts (default-options)]
                   #:select [select default-select])
  (fzf opts select))

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

(define (subprocess->lines exes . opts)
  (define exe (if (path? exes) exes (find-executable-path exes)))
  (define-values
    (sp p-output p-input p-error)
    (apply subprocess #f #f (current-error-port) exe opts))
  (close-output-port p-input)
  (subprocess-wait sp)
  (port->lines p-output))

;;;;;;;

(define (default-options)
  (list*
   (choice "open"
           (λ () (fzf #f (λ (s) (exec (~a "open '" s "'"))))))
   (choice "edit"
           (λ () (fzf #f (λ (s) (exec (~a "${EDITOR} '" s "'"))))))
   (append (tmux-windows))))

(define (tmux-windows)
  (define ws
    (subprocess->lines "tmux" "list-windows" "-F" "#I #T"))
  (for/list ([w (in-list ws)])
    (match-define (regexp #rx"^([0-9]+) (.*?)$" (list _ ids title)) w)
    (choice (~a "t " title) (λ () (exec (~a "tmux select-window -t @" ids))))))

(define (default-select s)
  (displayln s))

(provide term-main
         default-options
         default-select)
