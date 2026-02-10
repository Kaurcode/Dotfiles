(define-module (xiug config home services impure-symlinks)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:export (impure-symlinks-service-type
            impure-symlinks-service))

(define (impure-symlinks-program symlinks)
  (plain-file
   "impure-symlinks-activate.scm"
   (call-with-output-string
     (lambda (port)
       (pretty-print
        `(begin
           (use-modules (guix build utils)
                        (ice-9 pretty-print)
                        (ice-9 format)
                        (srfi srfi-13))

           (define symlinks ',symlinks)

           (define home
             (or (getenv "HOME")
                 (error "impure-symlinks: HOME is not set")))
           (define state-path
             (string-append home
                            "/.local/state/guix-home/impure-symlinks-state.scm"))

           (define (path-exists? p)
             (not (not (false-if-exception (lstat p)))))

           (define (dst->abs link)
             (if (string-prefix? "/" link)
                 link
                 (string-append home "/" link)))

           (define (ensure-parent-dir p)
             (mkdir-p (dirname p)))

           (define (symlink-target path)
             (false-if-exception (readlink path)))

           (define (assert-no-non-symlink dst)
             (when (path-exists? dst)
               (let ((st (lstat dst)))
                 (when (not (eq? 'symlink (stat:type st)))
                   (error (format #f
                                  "impure-symlinks: refusing to touch existing non-symlink at ~a"
                                  dst))))))

           (define (unique-pivot-name base)
             (string-append base ".~" (number->string (current-time)) ".new"))

           (define (atomic-replace-symlink dst target)
             (let ((pivot (unique-pivot-name dst)))
               (catch #t
                 (lambda ()
                   (when (path-exists? pivot)
                     (delete-file pivot))
                   (symlink target pivot)
                   (rename-file pivot dst))
                 (lambda (key . args)
                   (when (path-exists? pivot)
                     (false-if-exception (delete-file pivot)))
                   (apply throw key args)))))

           (define (read-state)
             (if (path-exists? state-path)
                 (call-with-input-file state-path
                   (lambda (port)
                     (let ((x (read port)))
                       (if (eof-object? x) '() x))))
                 '()))

           (define (atomic-write-state st)
             (ensure-parent-dir state-path)
             (let ((pivot (unique-pivot-name state-path)))
               (catch #t
                 (lambda ()
                   (call-with-output-file pivot
                     (lambda (port)
                       (pretty-print st port)))
                   (rename-file pivot state-path))
                 (lambda (key . args)
                   (when (path-exists? pivot)
                     (false-if-exception (delete-file pivot)))
                   (apply throw key args)))))

           (define (normalize-raw raw)
             (cond
               ((and (list? raw)
                     (or (null? raw)
                         (and (pair? (car raw))
                              (string? (car (car raw)))
                              (string? (cdr (car raw))))))
                raw)
               ((and (list? raw) (pair? raw) (list? (car raw)))
                (apply append raw))
               (else
                (error (format #f
                               "impure-symlinks: #:symlinks has unexpected shape: ~s"
                               raw)))))

           (define (validate-and-absolutize desired-raw)
             (map (lambda (x)
                    (unless (and (pair? x) (string? (car x)) (string? (cdr x)))
                      (error (format #f
                                     "impure-symlinks: invalid entry: ~s (expected (\"link\" . \"target\"))"
                                     x)))
                    (cons (dst->abs (car x)) (cdr x)))
                  desired-raw))

           (define (assert-owned-or-adoptable dst desired-target prev)
             (when (path-exists? dst)
               (let ((st (lstat dst)))
                 (when (eq? 'symlink (stat:type st))
                   (let* ((cur (symlink-target dst))
                          (prev-pair (assoc dst prev))
                          (prev-target (and prev-pair (cdr prev-pair))))
                     (cond
                       ((and cur (string=? cur desired-target)) #t)
                       ((and prev-target cur (string=? cur prev-target)) #t)
                       (else
                        (error (format #f
                                       "impure-symlinks: refusing to change ~a (symlink -> ~a); expected ~a or already desired ~a"
                                       dst
                                       (or cur "<unknown>")
                                       (or prev-target "<no-prev-state>")
                                       desired-target)))))))))

           (define (create-or-update dst desired-target prev)
             (ensure-parent-dir dst)
             (assert-no-non-symlink dst)
             (assert-owned-or-adoptable dst desired-target prev)
             (let ((cur (and (path-exists? dst)
                             (let ((st (lstat dst)))
                               (and (eq? 'symlink (stat:type st))
                                    (symlink-target dst))))))
               (when (not (and cur (string=? cur desired-target)))
                 (atomic-replace-symlink dst desired-target))))

           (define (remove-old dst old-target)
             (when (path-exists? dst)
               (let* ((st (lstat dst))
                      (cur (and (eq? 'symlink (stat:type st))
                                (symlink-target dst))))
                 (cond
                   ((not (eq? 'symlink (stat:type st)))
                    (error (format #f
                                   "impure-symlinks: expected symlink at ~a from previous state, but found non-symlink"
                                   dst)))
                   ((and cur (string=? cur old-target))
                    (delete-file dst))
                   (else
                    (error (format #f
                                   "impure-symlinks: refusing to remove ~a; current target is ~a but previous state target is ~a"
                                   dst (or cur "<unknown>") old-target)))))))

           ;; ---- main ----
           (let* ((prev (read-state))
                  (raw symlinks)
                  (desired-raw (normalize-raw raw))
                  (desired (validate-and-absolutize desired-raw)))

             (for-each
              (lambda (p)
                (let ((dst (car p)) (old-target (cdr p)))
                  (when (not (assoc dst desired))
                    (remove-old dst old-target))))
              prev)

             (for-each
              (lambda (p)
                (create-or-update (car p) (cdr p) prev))
              desired)

             (atomic-write-state desired)))
        port)))))

(define (impure-symlinks-activation-gexp symlinks)
  (with-imported-modules (source-module-closure '((guix build utils)))
    #~(lambda ()
        (primitive-load #$(impure-symlinks-program symlinks)))))

(define impure-symlinks-service-type
  (service-type
   (name 'impure-symlinks)
   (compose concatenate)
   (extend append)
   (extensions
    (list (service-extension
           home-activation-service-type
           (lambda (symlinks)
             (list (impure-symlinks-activation-gexp symlinks))))))
   (description
    "Create impure symlinks directly between arbitrary paths (not via the store), \
statefully and safely; atomic updates; avoids churn when unchanged.")))

(define* (impure-symlinks-service #:key (symlinks '()))
  (unless (list? symlinks)
    (error "impure-symlinks: #:symlinks must be a list of (LINK . TARGET) pairs"))
  (for-each
   (lambda (x)
     (unless (and (pair? x) (string? (car x)) (string? (cdr x)))
       (error (format #f
                      "impure-symlinks: invalid entry ~s; expected (\"link\" . \"target\")"
                      x))))
   symlinks)
  (service impure-symlinks-service-type symlinks))
