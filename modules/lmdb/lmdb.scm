(define-module (lmdb lmdb)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (system foreign-object)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 exceptions)
  #:export-syntax (with-cursor
                   with-env-and-txn
                   with-wrapped-cursor)
  #:export (;; Custom function setters
            set-compare!
            set-dupsort!
            ;; Environment flags
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
            +nodupdata+
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
            val-data-parse
            ;; Database
            dbi-open
            dbi-close
            ;; Actual ops
            get
            ref
            put
            put!
            set!
            del
            del!
            remove!
            ;; Cursor ops
            cursor-open
            cursor-close
            cursor-get
            cursor-first
            cursor-next
            cursor-last
            cursor-put
            cursor-del
            for-cursor
            ;; Stat
            stat?
            make-stat
            stat-depth
            stat-entries
            stat-pages
            dbi-stat
            env-stat
            ;; Helpers
            call-with-cursor
            call-with-env-and-txn
            call-with-wrapped-cursor))

(define liblmdb (load-foreign-library "liblmdb.so"))
;; (define liblmdb (load-foreign-library "/home/aartaka/.guix-profile/lib/liblmdb.so"))

(define* (foreign-fn name args #:optional (return-type int))
  "Generate `foreign-library-function' from a shorter form."
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
(define +nodupdata+ #x20)
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

(define %strerror (foreign-fn "mdb_strerror" (list int) '*))
(define (check-error err)
  (if (zero? err)
      #t
      (raise-exception (make-exception-with-message (pointer->string (%strerror err)))
                       #:continuable? #t)))

(define (set-compare! txn dbi function)
  (check-error
   ((foreign-fn "mdb_set_compare" `(* ,unsigned-int *))
    txn dbi (if (pointer? function)
                function
                (procedure->pointer int function '(* *))))))
(define (set-dupsort! txn dbi function)
  (check-error
   ((foreign-fn "mdb_set_dupsort" `(* ,unsigned-int *))
    txn dbi (if (pointer? function)
                function
                (procedure->pointer int function '(* *))))))
;; TODO: Do set-relfunc! and set-relctx! make any sense?

(define* (env-create #:key (maxdbs 1) maxreaders mapsize)
  "Make a new env and set the MAXDBS number (or 1)—maxdb number is mandatory.
Also set MAXREADERS and MAPSIZE, when provided."
  (let ((ptr (alloc-ptr)))
    (check-error ((foreign-fn "mdb_env_create" '(*)) ptr))
    (check-error
     ((foreign-fn "mdb_env_set_maxdbs" `(* ,unsigned-int))
      (dereference-pointer ptr) maxdbs))
    (when mapsize
      (check-error
       ((foreign-fn "mdb_env_set_mapsize" `(* ,size_t))
        (dereference-pointer ptr) mapsize)))
    (when maxreaders
      (check-error
       ((foreign-fn "mdb_env_set_maxreaders" `(* ,unsigned-int))
        (dereference-pointer ptr) maxreaders)))
    (dereference-pointer ptr)))
(define make-env env-create)

(define* (env-open env path #:optional (flags 0) (mode #o777))
  "Open the ENV at the PATH, creating the PATH if necessary."
  (unless (file-exists? path)
    (mkdir path))
  (check-error
   ((foreign-fn "mdb_env_open" `(* * ,unsigned-int ,unsigned-int))
    env (string->pointer path) flags mode)))
(define (env-close env)
  ((foreign-fn "mdb_env_close" '(*) void) env))
(define* (env-copy env path #:optional (flags #f))
  "Copy an ENV to a new PATH.
Useful to manage several PATHs with env copies at once."
  (unless (file-exists? path)
    (mkdir path))
  (check-error
   (if flags
       ((foreign-fn "mdb_env_copy2" `(* * ,unsigned-int))
        env (string->pointer path) flags)
       ((foreign-fn "mdb_env_copy" '(* *))
        env (string->pointer path)))))

(define* (txn-begin env #:optional (flags 0) (parent %null-pointer))
  (let ((ptr (alloc-ptr)))
    (check-error ((foreign-fn "mdb_txn_begin" `(* * ,unsigned-int *))
                  env parent flags ptr))
    (dereference-pointer ptr)))
(define (txn-abort txn)
  ((foreign-fn "mdb_txn_abort" `(*) void) txn))
(define (txn-reset txn)
  ((foreign-fn "mdb_txn_reset" `(*) void) txn))
(define (txn-commit txn)
  (check-error ((foreign-fn "mdb_txn_commit" `(*)) txn)))
(define (txn-renew txn)
  (check-error ((foreign-fn "mdb_txn_renew" `(*)) txn)))

(define-wrapped-pointer-type val
  val?
  wrap-val unwrap-val
  (lambda (v p)
    (format p "#<MDB_val of size ~d with ~x>"
            (val-size v) (pointer-address (val-data v)))))

(define (val-size val)
  "size_t of the VAL data."
  (first (parse-c-struct (unwrap-val val) `(,size_t *))))

(define (val-data val)
  "Raw pointer for the VAL data."
  (second (parse-c-struct (unwrap-val val) `(,size_t *))))

(define (val-data-string val)
  (pointer->string (val-data val) (val-size val)))

(define (val-data-bv val)
  (pointer->bytevector (val-data val) (val-size val)))

(define (val-data-parse val types)
  "VAL data parsed from C TYPES (via/akin to `parse-c-struct')."
  (parse-c-struct (val-data val) types))

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
  "Create a new value of SIZE from CONTENTS.
CONTENTS might be #f or
- A string.
- A bytevector.
- Another val (returned unaltered).
- A pointer.

SIZE is computed automatically for most of these cases, except
pointer. You have to explicitly provide the size for the pointer."
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

(define* (dbi-open txn #:optional (name #f) (flags +create+))
  "Create a new DBI and return it."
  (let* ((uint-size (sizeof unsigned-int))
         (ptr (alloc-ptr uint-size)))
    ;; FIXME: DBI is not a pointer, but rather unsigned-int.
    (check-error ((foreign-fn "mdb_dbi_open" `(* * ,unsigned-int *))
                  txn (if name
                          (string->pointer name)
                          %null-pointer)
                  flags ptr))
    (first (bytevector->uint-list
            (pointer->bytevector ptr uint-size)
            (endianness little) uint-size))))

(define (dbi-close env dbi)
  ((foreign-fn "mdb_dbi_close" `(* ,unsigned-int) void) env dbi))

(define %get (foreign-fn "mdb_get" `(* ,unsigned-int * *)))
(define* (get txn dbi key)
  "Get the val for the KEY (anything that `make-val' accepts)."
  (let* ((data-ptr (unwrap-val (make-val))))
    (check-error
     (%get txn dbi (unwrap-val (make-val key)) data-ptr))
    (wrap-val data-ptr)))
(define ref get)

(define %put (foreign-fn "mdb_put" `(* ,unsigned-int * * ,unsigned-int)))
(define* (put txn dbi key data #:optional (flags 0))
  "Put DATA (anything `make-val`-able) under key (ditto)."
  (check-error
   (%put
    txn dbi
    (unwrap-val (make-val key))
    (unwrap-val (make-val data))
    flags)))
(define put! put)
(define set! put)

(define %del (foreign-fn "mdb_del" `(* ,unsigned-int * *)))
(define* (del txn dbi key #:optional (data #f))
  "Remove the DATA under KEY."
  (check-error
   (%del txn dbi (unwrap-val (make-val key)) (unwrap-val (make-val data)))))
(define del! del)
(define remove! del)

(define (cursor-open txn dbi)
  "Open a new cursor and return it."
  (let ((cursor-ptr (alloc-ptr)))
    (check-error
     ((foreign-fn "mdb_cursor_open" `(* ,unsigned-int *))
      txn dbi cursor-ptr))
    (dereference-pointer cursor-ptr)))
(define (cursor-close cursor)
  ((foreign-fn "mdb_cursor_close" `(*) void) cursor))

(define %cursor-get (foreign-fn "mdb_cursor_get" `(* * * ,int)))
(define* (cursor-get cursor #:optional (op +get-current+) (original-key #f))
  "Set the CURSOR pointer according to the OP and return key-value pair.

The return value is a (KEY VALUE) list, where both KEY and VALUE are
val objects."
  (let* ((key (make-val original-key))
         (key-ptr (unwrap-val key))
         (data (make-val))
         (data-ptr (unwrap-val data)))
    (check-error
     (%cursor-get cursor key-ptr data-ptr op))
    (list key data)))
(define (cursor-first cursor)
  (cursor-get cursor +first+))
(define (cursor-last cursor)
  (cursor-get cursor +get-last+))
(define (cursor-next cursor)
  (cursor-get cursor +get-next+))
(define* (cursor-put cursor key val #:optional (flags 0))
  (let* ((key  (unwrap-val (make-val key)))
         (data (unwrap-val (make-val val))))
    (check-error
     ((foreign-fn "mdb_cursor_put" `(* * * ,unsigned-int))
      cursor key data flags))))
(define* (cursor-del cursor #:optional (flags 0))
  (check-error
   ((foreign-fn "mdb_cursor_del" `(* ,unsigned-int))
    cursor)))

(define (call-with-cursor txn dbi thunk)
  "Call thunk with the cursor established from TXN and DBI."
  (let* ((cursor (cursor-open txn dbi))
         (result (thunk cursor)))
    (cursor-close cursor)
    result))

(define-syntax-rule (with-cursor txn dbi (cursor) body ...)
  "Run BODY with CURSOR bound to the cursor at TXN/DBI."
  (call-with-cursor
   txn dbi
   (lambda (cursor)
     body ...)))

(define (for-cursor cursor thunk)
  "Walk the CURSOR, calling THUNK with entry's (KEY VALUE).
Stops at the last item.
Requires that DB has at least one entry, otherwise meaningless and
throwing errors."
  (let ((last-kv (cursor-last cursor))
        (first-kv (cursor-first cursor))
        (kv-eq? (lambda (kv kv2)
                  (and (bytevector=? (val-data-bv (first kv))
                                     (val-data-bv (first kv2)))
                       (bytevector=? (val-data-bv (second kv))
                                     (val-data-bv (second kv2)))))))
    (apply thunk first-kv)
    (unless (kv-eq? first-kv last-kv)
      (do ((kv (cursor-get cursor +get-next+)
               (cursor-next cursor)))
          ((kv-eq? kv last-kv)
           (apply thunk kv))
        (apply thunk kv)))))

(define-wrapped-pointer-type stat
  stat?
  wrap-stat unwrap-stat
  (lambda (s p)
    (format p "#<MDB_stat, depth ~d, ~d entries, ~s pages>"
            (stat-depth s) (stat-entries s) (stat-pages s))))

(define (make-stat)
  (wrap-stat
   (make-c-struct stat-types (list 0 0 0 0 0 0))))

(define stat-types (list unsigned-int unsigned-int size_t size_t size_t size_t))
(define (parse-stat s)
  (parse-c-struct (unwrap-stat s) stat-types))
(define (stat-depth s)
  (second (parse-stat s)))
(define (stat-entries s)
  (last (parse-stat s)))
(define (stat-pages s)
  "Returns a list of (BRANCH LEAF OVERFLOW) page counts for S stat."
  (let ((p (parse-stat s)))
    (list (third p) (fourth p) (fifth p))))

(define (dbi-stat txn dbi)
  (let ((stat (make-stat)))
    (check-error
     ((foreign-fn "mdb_stat" `(* ,unsigned-int *))
      txn dbi (unwrap-stat stat)))
    stat))

(define (env-stat env)
  (let ((stat (make-stat)))
    (check-error
     ((foreign-fn "mdb_env_stat" `(* *))
      env (unwrap-stat stat)))
    stat))

(define* (call-with-env-and-txn path thunk #:rest args)
  "Create the environment (with ARGS) and open PATH in it.
Run THUNK with this environment and a new transaction on it.
Commit the transaction and close the env aftwerwards."
  (let ((env (apply env-create args)))
    (env-open env path)
    (let* ((txn (txn-begin env))
           (result (thunk env txn)))
      (txn-commit txn)
      (env-close env)
      result)))

(define-syntax-rule (with-env-and-txn (path args ...) (env txn) body ...)
  "Run BODY with ENV and TXN bound to meaningful values.
ENV is open at PATH and created with ARGS."
  (call-with-env-and-txn
   path
   (lambda (env txn)
     body ...)
   args ...))


(define* (call-with-wrapped-cursor path name thunk #:rest args)
  "Run THUNK with (ENV TXN DBI CURSOR) created for it.
ARGS and PATH are for environment creation.
NAME (string or #f) is for DBI creation."
  (apply call-with-env-and-txn
         path
         (lambda (env txn)
           (let ((dbi (dbi-open txn name 0)))
             (call-with-cursor
              txn dbi
              (lambda (cursor)
                (thunk env txn dbi cursor)))))
         args))

(define-syntax-rule (with-wrapped-cursor (path name args ...) (env txn dbi cursor) body ...)
  "Run BODY with ENV and TXN bound to meaningful values.
ENV is open at PATH and created with ARGS.
NAME (string or #f) is for DBI creation."
  (call-with-wrapped-cursor
   path name
   (lambda (env txn dbi cursor)
     body ...)
   args ...))
