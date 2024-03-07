# Guile LMDB bindings

This library allows to interact with LMDB in Guile.
The focus of the library is making an otherwise unforgiving LMDB C API
more Scheme-ish and lenient.
Thus

-  Values can be anything: strings, bytevectors, or <code>val</code>
   objects wrapping arbitrary pointers.
- All the error codes are prettily displayed via Guile
  `error`, preventing the further operations on error.
- Lots of optional arguments for (opinionated) non-mandatory
  arguments.

## Getting Started

A simple listing for all the keys and values in the db might look like

``` scheme
(use-modules (system foreign))
(use-modules ((lmdb lmdb) #:prefix mdb:))
(mdb:with-env-and-txn ("path-to-db-dir/") (env txn)
 (let ((dbi (mdb:dbi-open txn #f 0)))
   (mdb:with-cursor txn dbi (cursor)
    (mdb:for-cursor
     cursor
     (lambda (key data)
       (display (mdb:val-data-string key))
       (display (mdb:val-data-parse data (list float float float float))))))))
```

`call-with-env-and-txn` and `for-cursor` are two procedures optimizing
the typical workflows: managing the environment+transaction; and going
through cursor values respectively. Other parts are visibly clunkier and closer to
the LMDB-provided APIs.

## Compatibility with LMDB conventions

All the APIs are following the LMDB names, with `mdb_` prefix stripped
off, and with underscores replaced with hyphen. Thus `mdb_env_create`
is `env-create`. This might be problematic, because some of the names
(`val`) are really short and thus might collide with existing
symbols. It's better to use this library with a prefix to avoid nasty
namespace errors:

``` scheme
(use-modules ((lmdb lmdb) #:prefix mdb:))
```

Significant diversions/additions from LMDB conventions are:

## Return values and errors

LMDB returns integer error codes from most of its functions to
indicate the success or failure of the operations (1). Important result
values are stored in the provided value pointers (2).

Neither of these is Lispy enough for this library.

- Success or failure of the operation (1) is managed by throwing
  errors. For failure, at least. Otherwise it returns `#t`, where no
  other return value fits better.
- Non-`#t` return values (2) are semantic—for operations that result
  in some value, Scheme procedure for it returns a meaningful
  value. For key/value functions, it's usually
  `val` objects.

## Special type: val

Most of LMDB key/value operations work with `MDB_val` objects, passing
them as keys and storing the needed data in them. Scheme API provides
a convenient wrapper: `val`. It's a full-blown object that wraps a
`MDB_val` pointer. Operations that `val` supports are:

<DL><dt> val? </dt> <dd> to test for <code>val</code>-ness.
<dt> (make-val contents [size]) </dt> <dd>
 to construct a new one. It accepts
 <UL><li> Strings.
  </li><li> Pointers.
  </li><li> Byte vectors.
  </li><li> In most cases, the size of the data is computed from the contents.
    <UL><li> Except for the pointer case. For that, pass the size of the data explicitly.
    </UL>
 </UL>
<dt> val-size </dt> <dd> to know the size of data within.
<dt> val-data </dt> <dd> to return data pointer.
<dt> val-data-string </dt> <dd> to get a string value for it.
<dt> val-data-bv </dt> <dd> to get a byte vector
<dt> (val-data-parse val types) </dt> <dd> to parse the data as a C structure, i.e.

``` scheme
;; Parse as four floats
(val-data-parse data (list float float float float))
```

## Another special type: stat

This is for DB statistics APIs: `dbi-stat` and `env-stat`. These
return a `stat-object`. You can use `stat-depth`, `stat-entries`, and
`stat-pages` to retrieve the statistics data.

## Further documentation

Most of the non-obvious functions/variables in the library are heavily
documented, so refer to their documentation via Guile built-in
facilities.
