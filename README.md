<h1>Guile LMDB bindings</h1>

<p>
This library allows to interact with LMDB in Guile.
The focus of the library is making an otherwise unforgiving LMDB C API
more Scheme-ish and lenient.
Thus

<UL><li> Values can be anything: strings, bytevectors, or <code>val</code> objects
  wrapping arbitrary pointers.
 <li> All the error codes are prettily displayed via Guile <code>error</code>,
  preventing the further operations on error.
 <li> Lots of optional arguments for (opinionated) non-mandatory
  arguments.
</UL>

</SECTION> <SECTION id=getting-started><h2><a href=#getting-started>Getting Started</a></h2>

<p>
A simple listing for all the keys and values in the db might look like

<pre lang=scheme>(use-modules (system foreign))
(use-modules ((lmdb lmdb) #:prefix mdb:))
(mdb:call-with-env-and-txn
 "path-to-db-dir/"
 (lambda (env txn)
   (let ((dbi (mdb:dbi-open txn #f 0)))
     (mdb:call-with-cursor
      txn dbi
      (lambda (cursor)
        (mdb:for-cursor
         cursor
         (lambda (key data)
           (display (mdb:val-data-string key))
           (display (mdb:val-data-parse data (list float float float float))))))))))
</pre>

<code>call-with-env-and-txn</code> and <code>for-cursor</code> are two procedures optimizing
the typical workflows: managing the environment+transaction; and going
through cursor values respectively. Other parts are visibly clunkier and closer to
the LMDB-provided APIs.

</SECTION> <SECTION id=lmdb-compatibility><h2><a href=#lmdb-compatibility>Compatibility with LMDB conventions</a></h2>

All the APIs are following the LMDB names, with <code>mdb_</code> prefix stripped
off, and with underscores replaced with hyphen. Thus <code>mdb_env_create</code>
is <code>env-create</code>. This might be problematic, because some of the names
(<code>val</code>) are really short and thus might collide with existing symbols. It's better to use this library with a prefix to avoid
nasty namespace errors:

<pre lang=scheme>(use-modules ((lmdb lmdb) #:prefix mdb:))</pre>

<p>
Significant diversions/additions from LMDB conventions are:

</SECTION> <SECTION id=return-values-and-errors><h2><a href=#return-values-and-errors>Return values and errors</a></h2>

<p>
LMDB returns integer error codes from most of its functions to
indicate the success or failure of the operations (1). Important result
values are stored in the provided value pointers (2).

<p>
Neither of these is Lispy enough for this library.

<UL><li> Success or failure of the operation (1) is managed by throwing
  errors. For failure, at least. Otherwise it returns <code>&num;t</code>, where no
  other return value fits better.
 <li> Non-<code>&num;t</code> return values (2) are semantic—for operations that result
  in some value, Scheme procedure for it returns a meaningful
  value. For key/value functions, it's usually
  <code>val</code> objects.
</UL>

</SECTION> <SECTION id=val><h2><a href=#val>Special type: <code>val</code></a></h2>

<p>
Most of LMDB key/value operations work with <code>MDB_val</code> objects, passing
them as keys and storing the needed data in them. Scheme API provides
a convenient wrapper: <code>val</code>. It's a full-blown object that wraps a
<code>MDB_val</code> pointer. Operations that <code>val</code> supports are:

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

<pre lang=scheme>;; Parse as four floats
(val-data-parse data (list float float float float))
</pre>

</SECTION> <SECTION id=stat><h2><a href=#stat>Another special type: <code>stat</code></a></h2>

<p>
This is for DB statistics APIs: <code>dbi-stat</code> and <code>env-stat</code>. These return a
<code>stat-object</code>. You can use <code>stat-depth</code>, <code>stat-entries</code>, and
<code>stat-pages</code> to retrieve the statistics data.

</SECTION> <SECTION id=further-documentation><h2><a href=#further-documentation>Further documentation</a></h2>

<p>
Most of the non-obvious functions/variables in the library are heavily
documented, so refer to their documentation via Guile built-in
facilities.
