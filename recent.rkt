#lang racket/base
(require racket/list
         racket/file
         racket/match)

(define recent.d (build-path (find-system-path 'home-dir) ".config" "recent"))
(define (ensure-recent.d!)
  (unless (directory-exists? recent.d)
    (make-directory recent.d)))

(define (recent-path mode) (build-path recent.d mode))

(define (file->list* p)
  (if (file-exists? p) (file->list p) '()))
(define (recent-read mode)
  (define h (make-hash))
  (for ([cv (file->list* (recent-path mode))])
    (match-define (cons cnt v) cv)
    (hash-update! h v (λ (x) (+ cnt x)) 0))
  (sort (hash->list h) >= #:key cdr))
(define (recent-readl mode)
  (map car (recent-read mode)))

(define (recent-write1! mode what)
  (ensure-recent.d!)
  (with-output-to-file (recent-path mode) #:exists 'append
    (λ ()
      (writeln (cons 1 what)))))
(define (recent-write! mode l)
  (ensure-recent.d!)
  (with-output-to-file (recent-path mode) #:exists 'replace
    (λ ()
      (for ([vc (in-list l)])
        (match-define (cons v cnt) vc)
        (when (> cnt 1) (writeln (cons cnt v)))))))

(module+ main
  (match-define (vector mode what) (current-command-line-arguments))
  (recent-write1! mode what)
  (when (or (getenv "RECENT_REFRESH")
            (<= (expt 2 12) (file-size (recent-path mode))))
    (recent-write! mode (recent-read mode))))

(provide recent-read
         recent-readl)

