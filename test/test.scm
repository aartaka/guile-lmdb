(define-module (lmdb tests)
  #:use-module (srfi srfi-64)
  #:use-module (system foreign)
  #:use-module ((lmdb lmdb) #:prefix mdb:))

(test-begin "env-create")
(define env (mdb:env-create 10))
(test-assert env)
(test-end "env-create")

(test-group-with-cleanup "env-open-and-copy-create-dir"
  (when (file-exists? "/tmp/lmbd-test/")
    (rmdir "/tmp/lmbd-test/"))
  (when (file-exists? "/tmp/lmbd-test2/")
    (rmdir "/tmp/lmbd-test2/"))

  (test-assert (mdb:env-open env "/tmp/lmbd-test/"))
  (test-assert (file-exists? "/tmp/lmbd-test/"))

  (test-assert (mdb:env-copy env "/tmp/lmbd-test2/"))
  (test-assert (file-exists? "/tmp/lmbd-test2/"))
  (begin
    (rmdir "/tmp/lmbd-test/")
    (rmdir "/tmp/lmbd-test2/")))

(test-begin "txn-begin")
(define txn (mdb:txn-begin env))
(test-assert txn)
(test-begin "txn-begin")

(test-group "txn-ops"
  (let ((operatable-txn (mdb:txn-begin env)))
    (test-assert (mdb:txn-reset operatable-txn))
    (test-assert (mdb:txn-renew operatable-txn))
    (test-assert (mdb:txn-commit operatable-txn))
    (test-assert (mdb:txn-abort (mdb:txn-begin env)))))

(test-begin "dbi-open")
(define dbi (mdb:dbi-open txn "data" +create+))
(test-assert dbi)
;; Same as above, but with implied args.
(test-assert dbi (mdb:dbi-open txn "data"))
(test-end "dbi-open")

(test-group "get-empty"
  (test-error (mdb:get txn dbi "test")))

(test-group "string-data"
  (test-assert (mdb:put txn dbi "string" "string"))
  (test-assert (string=? "string" (mdb:val-data-string (mdb:get txn dbi "string")))))

(test-group "bv-data"
  (let ((bv (make-bytevector 5 8)))
    (test-assert (mdb:put txn dbi "bv" bv))
    (test-assert (bytevector? (mdb:val-data-bv (mdb:get txn dbi "bv"))))
    (test-assert (bytevector=? bv (mdb:val-data-bv (mdb:get txn dbi "bv"))))))

(test-group "struct-data"
  (let ((s (make-c-struct (list int float) '(1 1.1))))
    (test-assert
        (mdb:put txn dbi "struct"
                 (mdb:make-val s (+ (sizeof int) (sizeof float)))))
    (let ((vals (mdb:val-data-parse (mdb:get txn dbi "bv")
                                    (list int float))))
      (test-eqv (first vals) 1)
      (test-eqv (second vals) 1.1))))

(test-group "cursor"
  (let* ((cursor (mdb:cursor-open txn dbi))
         (fst (mdb:cursor-first cursor))
         (nxt (mdb:cursor-next cursor))
         (lst (mdb:cursor-last cursor)))
    (test-equal "string" (mdb:val-data-string (first fst)))
    (test-equal "bv" (mdb:val-data-string (first nxt)))
    (test-equal "struct" (mdb:val-data-string (first lst)))
    (mdb:cursor-close cursor)))

(test-group "for-cursor"
  (let ((cursor (mdb:cursor-open txn dbi)))
    (mdb:for-cursor
     cursor
     (lambda (key value)
       (test-assert (member (mdb:val-data-string key)
                            '("string" "bv" "struct")))))
    (mdb:cursor-close cursor)))

(test-group "get-bogus"
  (test-error (mdb:get txn dbi "foo"))
  (test-error (mdb:get txn dbi (mdb:make-val %null-pointer (sizeof '*)))))

(test-group "del"
  (test-assert (mdb:del txn dbi "string"))
  (test-assert (mdb:del txn dbi "bv"))
  (test-assert (mdb:del txn dbi "struct"))
  (test-error (mdb:get txn dbi "string"))
  (test-error (mdb:get txn dbi "bv"))
  (test-error (mdb:get txn dbi "struct")))

(test-group "env-close"
  (mdb:env-close env))
