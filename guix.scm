(use-modules
 (gnu packages guile)
 (gnu packages databases)
 (guix packages)
 (guix gexp)
 (guix build-system guile)
 ((guix licenses) #:prefix license:))

(define-public guile-lmdb
  (package
   (name "guile-lmdb")
   (version "0.0.1")
   (source (local-file (dirname (current-filename)) #:recursive? #t))
   (build-system guile-build-system)
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
   (license license:gpl3)))

guile-lmdb
