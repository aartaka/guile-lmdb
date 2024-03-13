;; Run with
;; guile --debug -L . test/test.scm
(define-module (lmdb tests)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module ((lmdb lmdb) #:prefix mdb:))

(test-begin "env-create")
(define env (mdb:env-create #:maxdbs 10))
(test-assert env)
(test-end "env-create")

(test-begin "env-open-and-copy-create-dir")
(when (file-exists? "/tmp/lmbd-test/")
  (when (file-exists? "/tmp/lmbd-test/data.mdb")
    (delete-file "/tmp/lmbd-test/data.mdb"))
  (when (file-exists? "/tmp/lmbd-test/lock.mdb")
    (delete-file "/tmp/lmbd-test/lock.mdb"))
  (rmdir "/tmp/lmbd-test/"))
(when (file-exists? "/tmp/lmbd-test2/")
  (when (file-exists? "/tmp/lmbd-test2/data.mdb")
    (delete-file "/tmp/lmbd-test2/data.mdb"))
  (when (file-exists? "/tmp/lmbd-test2/lock.mdb")
    (delete-file "/tmp/lmbd-test2/lock.mdb"))
  (rmdir "/tmp/lmbd-test2/"))

(test-assert (mdb:env-open env "/tmp/lmbd-test/"))
(test-assert (file-exists? "/tmp/lmbd-test/"))

(test-assert (mdb:env-copy env "/tmp/lmbd-test2/"))
(test-assert (file-exists? "/tmp/lmbd-test2/"))
(test-end "env-open-and-copy-create-dir")

(test-begin "txn-ops")
(define operatable-txn (mdb:txn-begin env))
(test-assert (mdb:txn-reset operatable-txn))
;; Fails.
;; (test-assert (mdb:txn-renew operatable-txn))
(test-assert (mdb:txn-commit operatable-txn))
(test-assert (mdb:txn-abort (mdb:txn-begin env)))
(test-end "txn-ops")

(test-begin "txn-begin")
(define txn (mdb:txn-begin env))
(test-assert txn)
(test-end "txn-begin")

(test-begin "dbi-open")
(define dbi (mdb:dbi-open txn "data" mdb:+create+))
(test-assert dbi)
;; Same as above, but with implied args.
(test-assert dbi (mdb:dbi-open txn "data"))
(test-end "dbi-open")

(test-begin "get-empty")
(test-error (mdb:get txn dbi "test"))
(test-end "get-empty")

(test-begin "string-data")
(test-assert (mdb:put txn dbi "string" "string"))
(test-assert (string=? "string" (mdb:val-data-string (mdb:get txn dbi "string"))))
(test-end "string-data")

(test-begin "bv-data")
(define bv (make-bytevector 5 8))
(test-assert (mdb:put txn dbi "bv" bv))
(test-assert (bytevector? (mdb:val-data-bv (mdb:get txn dbi "bv"))))
(test-assert (bytevector=? bv (mdb:val-data-bv (mdb:get txn dbi "bv"))))
(test-end "bv-data")

(test-begin "struct-data")
(define s (make-c-struct (list int float) '(1 1.1)))
(test-assert
    (mdb:put txn dbi "struct"
             (mdb:make-val s (+ (sizeof int) (sizeof float)))))
(define vals (mdb:val-data-parse (mdb:get txn dbi "struct")
                                 (list int float)))
(test-eqv 1 (first vals))
(test-assert (< 1.1 (second vals) 1.12))
(test-end "struct-data")

(test-begin "cursor")
(let* ((cursor (mdb:cursor-open txn dbi))
       (fst (mdb:cursor-first cursor))
       (nxt (mdb:cursor-next cursor))
       ;; Because cursor-last was not exported until recently.
       (lst (mdb:cursor-get cursor mdb:+get-last+)))
  (test-assert (member (mdb:val-data-string (first fst))
                       '("string" "bv" "struct")))
  (test-assert (member (mdb:val-data-string (first nxt))
                       '("string" "bv" "struct")))
  (test-assert (member (mdb:val-data-string (first lst))
                       '("string" "bv" "struct")))
  (mdb:cursor-close cursor))
(test-end "cursor")

(test-begin "for-cursor")
(let ((cursor (mdb:cursor-open txn dbi)))
  (mdb:for-cursor
   cursor
   (lambda (key value)
     (test-assert (member (mdb:val-data-string key)
                          '("string" "bv" "struct")))))
  (mdb:cursor-close cursor))
(test-end "for-cursor")

(test-begin "get-bogus")
(test-error (mdb:get txn dbi "foo"))
(test-end "get-bogus")

(test-begin "del")
(test-assert (mdb:del txn dbi "string"))
(test-assert (mdb:del txn dbi "bv"))
(test-assert (mdb:del txn dbi "struct"))
(test-error (mdb:get txn dbi "string"))
(test-error (mdb:get txn dbi "bv"))
(test-error (mdb:get txn dbi "struct"))
(test-end "del")

(test-begin "env-close")
(mdb:env-close env)
(test-end "env-close")
