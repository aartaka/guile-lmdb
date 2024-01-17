(define-module (guile-lmdb-package)
 #:use-module (gnu packages guile)
 #:use-module (gnu packages databases)
 #:use-module (guix packages)
 #:use-module (guix gexp)
 #:use-module (guix utils)
 #:use-module (guix build-system guile)
 #:use-module (guix git-download)
 #:use-module ((guix licenses) #:prefix license:))

(define-public guile-lmdb
  (package
    (name "guile-lmdb")
    (version "0.0.1")
    (source (local-file (dirname (dirname (current-source-directory)))
                        #:recursive? #t
                        #:select? (or (git-predicate
                                       (dirname
                                        (dirname (current-source-directory))))
                                      (const #t))))
    (build-system guile-build-system)
    (arguments
     '(#:source-directory "modules"
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'remove-channel-file
                    (lambda _
                      (delete-file ".guix/modules/guile-lmdb-package.scm")
                      #t)))))
    (native-inputs (list guile-3.0))
    (inputs (list guile-3.0 lmdb))
    (home-page "https://github.com/aartaka/guile-lmdb")
    (synopsis "Bindings for LMDB (Lightning Memory-Mapped Database) in Guile.")
    (description "Scheme wrapper around liblmdb.so.
Most name are the same as LMDB ones, except for prefix absence.
Several conveniences are added on top:
@itemize
@item @code{for-cursor} procedure for cursor iteration.
@item @code{call-with-env-and-txn} wrapper.
@item @code{val} and @code{stat} types.
@item Error signaling instead of integer return values.
@end itemize\n")
    (license license:gpl3+)))

guile-lmdb
