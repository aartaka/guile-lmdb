(define-module (guile-lmdb-package)
 #:use-module (gnu packages guile)
 #:use-module (gnu packages databases)
 #:use-module (guix packages)
 #:use-module (guix gexp)
 #:use-module (guix utils)
 #:use-module (guix build-system guile)
 #:use-module (guix git-download)
 #:use-module ((guix licenses) #:prefix license:))

(define-public guile-lmdb-git
  (package
    (name "guile-lmdb-git")
    (version "0.0.1")
    (source (local-file ".."
                        "guile-lmdb-git-checkout"
                        #:recursive? #t
                        #:select? (or (git-predicate (dirname (current-source-directory)))
                                      (const #t))))
    (build-system guile-build-system)
    (arguments
     '(#:source-directory "modules"
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'substitute-lmdb-so
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((lmdb (string-append (assoc-ref inputs "lmdb")
                                                 "/lib/liblmdb.so")))
                        (substitute*
                            '("modules/lmdb/lmdb.scm")
                          (("liblmdb.so")
                           lmdb))
                        #t))))))
    (native-inputs (list guile-3.0))
    (inputs (list guile-3.0 lmdb))
    (home-page "https://github.com/aartaka/guile-lmdb")
    (synopsis "Bindings for LMDB (Lightning Memory-Mapped Database) in Guile.")
    (description "Scheme wrapper around liblmdb.so.
Most name are the same as LMDB ones, except for prefix absence.
Several conveniences are added on top:
@itemize
@item @code{call-with-env-and-txn} and @code{call-with-cursor} wrappers.
@item @code{for-cursor} procedure for cursor iteration.
@item @code{val} and @code{stat} types.
@item Error signaling instead of integer return values.
@end itemize\n")
    (license license:gpl3+)))

guile-lmdb-git
