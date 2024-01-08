(define-module (lmdb)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (system foreign-object)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (;; Environment flags
            +fixedmap+
            +nosubdir+
            +nosync+
            +rdonly+
            +nometasync+
            +writemap+
            +mapasync+
            +notls+
            +nolock+
            +nordahead+
            +nomeminit+
            ;; DB flags
            +reversekey+
            +dupsort+
            +integerkey+
            +dupfixed+
            +integerdup+
            +reversedup+
            +create+
            ;; Write flags
            +nooverwrite+
            +noodupdata+
            +current+
            +reserve+
            +append+
            +appenddup+
            +multiple+
            ;; Cursor flags
            +first+
            +first-dup+
            +get-both+
            +get-both-range+
            +get-current+
            +get-multiple+
            +get-last+
            +get-last-dup+
            +get-next+
            +get-next-dup+
            +get-next-multiple+
            +get-next-nodup+
            +get-prev+
            +get-prev-dup+
            +get-prev-nodup+
            +get-set+
            +get-set-key+
            +get-set-range+
            ;; Environment
            env-create
            env-open
            env-close
            env-copy
            ;; Transaction
            txn-begin
            txn-abort
            txn-reset
            txn-commit
            txn-renew
            ;; Valus
            val?
            make-val
            wrap-val
            unwrap-val
            val-size
            val-data
            val-data-string
            val-data-bv
            ;; Database
            dbi-open
            dbi-close
            ;; Actual ops
            get
            put
            del
            ;; Cursor ops
            cursor-open
            cursor-get
            cursor-first
            cursor-next
            cursor-put
            cursor-del
            ;; Helpers
            call-with-env))

(define liblmdb (load-foreign-library "liblmdb.so"))

(define (foreign-fn name args return-type)
  (foreign-library-function
   liblmdb name
   #:return-type return-type
   #:arg-types args))

(define* (alloc-ptr #:optional (size (sizeof '*)))
  (bytevector->pointer (make-bytevector size)))

;; Env flags
(define +fixedmap+ #x01)
(define +nosubdir+ #x4000)
(define +nosync+ #x10000)
(define +rdonly+ #x20000)
(define +nometasync+ #x40000)
(define +writemap+ #x80000)
(define +mapasync+ #x100000)
(define +notls+ #x200000)
(define +nolock+ #x400000)
(define +nordahead+ #x800000)
(define +nomeminit+ #x1000000)
;; DB flags
(define +reversekey+ #x02)
(define +dupsort+ #x04)
(define +integerkey+ #x08)
(define +dupfixed+ #x10)
(define +integerdup+ #x20)
(define +reversedup+ #x40)
(define +create+ #x40000)
;; Write flags
(define +nooverwrite+ #x10)
(define +noodupdata+ #x20)
(define +current+ #x40)
(define +reserve+ #x10000)
(define +append+ #x20000)
(define +appenddup+ #x40000)
(define +multiple+ #x80000)
;; Cursor operations
(define +first+ 0)
(define +first-dup+ 1)
(define +get-both+ 2)
(define +get-both-range+ 3)
(define +get-current+ 4)
(define +get-multiple+ 5)
(define +get-last+ 6)
(define +get-last-dup+ 7)
(define +get-next+ 8)
(define +get-next-dup+ 9)
(define +get-next-multiple+ 10)
(define +get-next-nodup+ 11)
(define +get-prev+ 12)
(define +get-prev-dup+ 13)
(define +get-prev-nodup+ 14)
(define +get-set+ 15)
(define +get-set-key+ 16)
(define +get-set-range+ 17)

(define (check-error err)
  (if (zero? err)
      #t
      (error (pointer->string ((foreign-fn "mdb_strerror" (list int) '*) err)))))

(define* (env-create #:optional (maxdbs 1))
  (let ((ptr (alloc-ptr)))
    (check-error ((foreign-fn "mdb_env_create" '(*) int) ptr))
    ;; FIXME: `check-error' fails here.
    (check-error
     ((foreign-fn "mdb_env_set_maxdbs" `(* ,unsigned-int) int)
      (dereference-pointer ptr) maxdbs))
    (dereference-pointer ptr)))

(define* (env-open env path #:optional (flags 0) (mode #o777))
  (unless (file-exists? path)
    (mkdir path))
  (check-error
   ((foreign-fn "mdb_env_open" `(* * ,unsigned-int ,unsigned-int) int)
    env (string->pointer path) flags mode)))
(define (env-close env)
  ((foreign-fn "mdb_env_close" '(*) void) env))
(define* (env-copy env path #:optional (flags #f))
  (unless (file-exists? path)
    (mkdir path))
  (check-error
   (if flags
       ((foreign-fn "mdb_env_copy2" `(* * ,unsigned-int) int)
        env (string->pointer path) flags)
       ((foreign-fn "mdb_env_copy" '(* *) int)
        env (string->pointer path)))))

(define* (txn-begin env #:optional (flags 0) (parent %null-pointer))
  (let ((ptr (alloc-ptr)))
    (check-error ((foreign-fn "mdb_txn_begin" `(* * ,unsigned-int *) int)
                  env parent flags ptr))
    (dereference-pointer ptr)))
(define (txn-abort txn)
  ((foreign-fn "mdb_txn_abort" `(*) void) txn))
(define (txn-reset txn)
  ((foreign-fn "mdb_txn_reset" `(*) void) txn))
(define (txn-commit txn)
  (check-error ((foreign-fn "mdb_txn_commit" `(*) int) txn)))
(define (txn-renew txn)
  (check-error ((foreign-fn "mdb_txn_renew" `(*) int) txn)))

(define-wrapped-pointer-type val
  val?
  wrap-val unwrap-val
  (lambda (v p)
    (format p "#<MDB_val of size ~d with ~x>"
            (val-size v) (pointer-address (val-data v)))))

(define (val-size val)
  (first (parse-c-struct (unwrap-val val) `(,size_t *))))

(define (val-data val)
  (second (parse-c-struct (unwrap-val val) `(,size_t *))))

(define (val-data-string val)
  (pointer->string (val-data val) (val-size val)))

(define (val-data-bv val)
  (pointer->bytevector (val-data val) (val-size val)))

(define* (make-val #:optional
                   (contents #f)
                   (size (cond
                          ((eq? #f contents)
                           0)
                          ((string? contents)
                           (bytevector-length (string->utf8 contents)))
                          ((bytevector? contents)
                           (bytevector-length contents))
                          ((val? contents)
                           (val-size contents))
                          (else
                           (error "Cannot determine the size of "
                                  contents ", provide it manually")))))
  (if (val? contents)
      contents
      (wrap-val
       (make-c-struct
        `(,size_t *)
        (list size (cond
                    ((eq? #f contents)
                     %null-pointer)
                    ((string? contents)
                     (string->pointer contents))
                    ((bytevector? contents)
                     (bytevector->pointer contents))
                    ((pointer? contents)
                     contents)))))))

(define* (dbi-open txn name #:optional (flags +create+))
  (let ((ptr (alloc-ptr (sizeof unsigned-int))))
    ;; FIXME: DBI is not a pointer, but rather unsigned-int.
    (check-error ((foreign-fn "mdb_dbi_open" `(* * ,unsigned-int *) int)
                  txn (string->pointer name) flags ptr))
    (first (bytevector->uint-list
            (pointer->bytevector ptr (sizeof unsigned-int))
            (endianness little) (sizeof unsigned-int)))))

(define (dbi-close env dbi)
  ((foreign-fn "mdb_dbi_close" `(* ,unsigned-int) void) env dbi))

(define* (get txn dbi key)
  (let* ((data-ptr (unwrap-val (make-val))))
    (check-error
     ((foreign-fn "mdb_get" `(* ,unsigned-int * *) int)
      txn dbi (unwrap-val (make-val key)) data-ptr))
    (wrap-val data-ptr)))
(define* (put txn dbi key data #:optional (flags 0))
  (check-error
   ((foreign-fn "mdb_put" `(* ,unsigned-int * * ,unsigned-int) int)
    txn dbi
    (unwrap-val (make-val key))
    (unwrap-val (make-val data))
    flags)))
(define (del txn dbi key data)
  (check-error
   ((foreign-fn "mdb_del" `(* ,unsigned-int * *) int)
    txn dbi
    (unwrap-val (make-val key))
    (unwrap-val (make-val data)))))

(define (cursor-open txn dbi)
  (let ((cursor-ptr (alloc-ptr)))
    (check-error
     ((foreign-fn "mdb_cursor_open" `(* ,unsigned-int *) int)
      txn dbi cursor-ptr))
    (dereference-pointer cursor-ptr)))

(define* (cursor-get cursor #:optional (op +get-current+))
  (let* ((key (make-val))
         (key-ptr (unwrap-val key))
         (data (make-val))
         (data-ptr (unwrap-val data)))
    (check-error
     ((foreign-fn "mdb_cursor_get" `(* * * ,int) int)
      cursor key-ptr data-ptr op))
    (list key data)))
(define (cursor-first cursor)
  (cursor-get cursor +first+))
(define (cursor-next cursor)
  (cursor-get cursor +get-next+))
(define* (cursor-put cursor key val #:optional (flags 0))
  (let* ((key  (unwrap-val (make-val key)))
         (data (unwrap-val (make-val val))))
    (check-error
     ((foreign-fn "mdb_cursor_put" `(* * * ,unsigned-int) int)
      cursor key data flags))))
(define* (cursor-del cursor #:optional (flags 0))
  (check-error
   ((foreign-fn "mdb_cursor_del" `(* ,unsigned-int) int)
    cursor)))

(define (call-with-env path thunk)
  (let ((env (env-create)))
    (thunk env)
    (env-close env)))
