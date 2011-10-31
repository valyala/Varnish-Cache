================================
Changes from 3.0.1 rc 1 to 3.0.1
================================

Varnishd
--------

- Fix crash in streaming code.

- Add `fallback` director, as a variant of the `round-robin`
  director.

- The parameter `http_req_size` has been reduced on 32 bit machines.

VCL
---

- Disallow error in the `vcl_init` and `vcl_fini` VCL functions.

varnishncsa
-----------

- Fixed crash when using `-X`.

- Fix error when the time to first byte was in the format string.

Other
-----

- Documentation updates

================================
Changes from 3.0.0 to 3.0.1 rc 1
================================

Varnishd
--------

- Avoid sending an empty end-chunk when sending bodyless responsed.

- `http_resp_hdr_len` and `http_req_hdr_len` were set to too low
  values leading to clients receiving `HTTP 400 Bad Request` errors.
  The limit has been increased and the error code is now `HTTP 413
  Request entity too large`.

- Objects with grace or keep set were mistakenly considered as
  candidates for the transient storage.  They now have their grace and
  keep limited to limit the memory usage of the transient stevedore.

- If a request was restarted from `vcl_miss` or `vcl_pass` it would
  crash.  This has been fixed.  `Bug #965`_.

- Only the first few clients waiting for an object from the backend
  would be woken up when object arrived and this lead to some clients
  getting stuck for a long time.  This has now been fixed. `Bug #963`_.

- The `hash` and `client` directors would mistakenly retry fetching an
  object from the same backend unless health probes were enabled.
  This has been fixed and it will now retry a different backend.

.. _bug #965: http://varnish-cache.org/trac/ticket/965
.. _bug #963: http://varnish-cache.org/trac/ticket/963

VCL
---

- Request specific variables such as `client.*` and `server.*` are now
  correctly marked as not available in `vcl_init` and `vcl_fini`.

- The VCL compiler would fault if two IP comparisons were done on the
  same line.  This now works correctly.  `Bug #948`_.

.. _bug #948: http://varnish-cache.org/trac/ticket/948

varnishncsa
-----------

- Add support for logging arbitrary request and response headers.

- Fix crashes if `hitmiss` and `handling` have not yet been set.

- Avoid printing partial log lines if there is an error in a format
  string.

- Report user specified format string errors better.

varnishlog
----------

- `varnishlog -r` now works correctly again and no longer opens the
  shared log file of the running Varnish.

Other
-----

- Various documentation updates.

- Minor compilation fixes for newer compilers.

- A bug in the ESI entity replacement parser has been fixed.  `Bug
  #961`_.

- The ABI of vmods are now checked.  This will require a rebuild of
  all vmods against the new version of Varnish.

.. _bug #961: http://varnish-cache.org/trac/ticket/961

================================
Changes from 3.0 beta 2 to 3.0.0
================================

Varnishd
--------

- Avoid sending an empty end-chunk when sending bodyless responsed.

VCL
---

- The `synthetic` keyword has now been properly marked as only
  available in `vcl_deliver`.  `Bug #936`_.

.. _bug #936: http://varnish-cache.org/trac/ticket/936

varnishadm
----------

- Fix crash if the secret file was unreadable.  `Bug #935`_.

- Always exit if `varnishadm` can't connect to the backend for any
  reason.

.. _bug #935: http://varnish-cache.org/trac/ticket/935

=====================================
Changes from 3.0 beta 1 to 3.0 beta 2
=====================================

Varnishd
--------

- thread_pool_min and thread_pool_max now each refer to the number of
  threads per pool, rather than being inconsistent as they were
  before.

- 307 Temporary redirect is now considered cacheable.  `Bug #908`_.

- The `stats` command has been removed from the CLI interface.  With
  the new counters, it would mean implementing more and more of
  varnishstat in the master CLI process and the CLI is
  single-threaded so we do not want to do this work there in the first
  place.  Use varnishstat instead.

.. _bug #908: http://varnish-cache.org/trac/ticket/908

VCL
---

- VCL now treats null arguments (unset headers for instance) as empty
  strings.  `Bug #913`_.

- VCL now has vcl_init and vcl_fini functions that are called when a
  given VCL has been loaded and unloaded.

- There is no longer any interpolation of the right hand side in bans
  where the ban is a single string.  This was confusing and you now
  have to make sure bits are inside or outside string context as
  appropriate.

- Varnish is now stricter in enforcing no duplication of probes,
  backends and ACLs.

.. _bug #913: http://varnish-cache.org/trac/ticket/913

varnishncsa
-----------

- varnishncsa now ignores piped requests, since we have no way of
  knowing their return status.

VMODs
-----

- The std module now has proper documentation, including a manual page

================================
Changes from 2.1.5 to 3.0 beta 1
================================

Upcoming changes
----------------

- The interpretation of bans will change slightly between 3.0 beta 1
  and 3.0 release.  Currently, doing ``ban("req.url == req.url")``
  will cause the right hand req.url to be interpreted in the context
  of the request creating the ban.  This will change so you will have
  to do ``ban("req.url == " + req.url)`` instead.  This syntax already
  works and is recommended.

Varnishd
--------

- Add streaming on ``pass`` and ``miss``.  This is controlled by the
  ``beresp.do_stream`` boolean.  This includes support for
  compression/uncompression.
- Add support for ESI and gzip.
- Handle objects larger than 2G.
- HTTP Range support is now enabled by default
- The ban lurker is enabled by default
- if there is a backend or director with the name ``default``, use
  that as the default backend, otherwise use the first one listed.
- Add many more stats counters.  Amongst those, add per storage
  backend stats and per-backend statistics.
- Syslog the platform we are running on
- The ``-l`` (shared memory log file) argument has been changed,
  please see the varnishd manual for the new syntax.
- The ``-S`` and ``-T`` arguments are now stored in the shmlog
- Fix off-by-one error when exactly filling up the workspace.  `Bug #693`_.
- Make it possible to name storage backends.  The names have to be
  unique.
- Update usage output to match the code.  `Bug #683`_
- Add per-backend health information to shared memory log.
- Always recreate the shared memory log on startup.
- Add a ``vcl_dir`` parameter.  This is used to resolve relative path
  names for ``vcl.load`` and ``include`` in .vcl files.
- Make it possible to specify ``-T :0``.  This causes varnishd to look
  for a free port automatically.  The port is written in the shared
  memory log so varnishadm can find it.
- Classify locks into kinds and collect stats for each kind,
  recording the data in the shared memory log.
- Auto-detect necessary flags for pthread support and ``VCC_CC``
  flags.  This should make Varnish somewhat happier on Solaris.  `Bug
  #663`_
- The ``overflow_max`` parameter has been renamed to ``queue_max``.
- If setting a parameter fails, report which parameter failed as this
  is not obvious during startup.
- Add a parameter named ``shortlived``.  Objects whose TTL is less
  than the parameter go into transient (malloc) storage.
- Reduce the default ``thread_add_delay`` to 2ms.
- The ``max_esi_includes`` parameter has been renamed to
  ``max_esi_depth``.
- Hash string components are now logged by default.
- The default connect timeout parameter has been increased to 0.7
  seconds.
- The ``err_ttl`` parameter has been removed and is replaced by a
  setting in default.vcl.
- The default ``send_timeout`` parameter has been reduced to 1 minute.
- The default ``ban_lurker`` sleep has been set to 10ms.
- When an object is banned, make sure to set its grace to 0 as well.
- Add ``panic.show`` and ``panic.clear`` CLI commands.
- The default ``http_resp_hdr_len`` and ``http_req_hdr_len`` has been
  increased to 2048 bytes.
- If ``vcl_fetch`` results in ``restart`` or ``error``, close the
  backend connection rather than fetching the object.
- If allocating storage for an object, try reducing the chunk size
  before evicting objects to make room.  `Bug #880`_
- Add ``restart`` from ``vcl_deliver``.  `Bug #411`_
- Fix an off-by-up-to-one-minus-epsilon bug where if an object from
  the backend did not have a last-modified header we would send out a
  304 response which did include a ``Last-Modified`` header set to
  when we received the object.  However, we would compare the
  timestamp to the fractional second we got the object, meaning any
  request with the exact timestamp would get a ``200`` response rather
  than the correct ``304``.
- Fix a race condition in the ban lurker where a serving thread and
  the lurker would both look at an object at the same time, leading to
  Varnish crashing.
- If a backend sends a ``Content-Length`` header and we are streaming and
  we are not uncompressing it, send the ``Content-Length`` header on,
  allowing browsers to diplay a progress bar.
- All storage must be at least 1M large.  This is to prevent
  administrator errors when specifying the size of storage where the
  admin might have forgotten to specify units.

.. _bug #693: http://varnish-cache.org/trac/ticket/693
.. _bug #683: http://varnish-cache.org/trac/ticket/683
.. _bug #663: http://varnish-cache.org/trac/ticket/663
.. _bug #880: http://varnish-cache.org/trac/ticket/880
.. _bug #411: http://varnish-cache.org/trac/ticket/411
.. _bug #693: http://varnish-cache.org/trac/ticket/693

Tools
-----

common
******

- Add an ``-m $tag:$regex`` parameter, used for selecting some
  transactions.  The parameter can be repeated, in which case it is
  logically and-ed together.

varnishadm
**********

- varnishadm will now pick up the -S and -T arguments from the shared
  memory log, meaning just running it without any arguments will
  connect to the running varnish.  `Bug #875`_
- varnishadm now accepts an -n argument to specify the location of the
  shared memory log file
- add libedit support

.. _bug #875: http://varnish-cache.org/trac/ticket/875

varnishstat
***********

- reopen shared memory log if the varnishd process is restarted.
- Improve support for selecting some, but not all fields using the
  ``-f`` argument. Please see the documentation for further details on
  the use of ``-f``.
- display per-backend health information

varnishncsa
***********

- Report error if called with ``-i`` and ``-I`` as they do not make
  any sense for varnishncsa.
- Add custom log formats, specified with ``-F``.  Most of the Apache
  log formats are supported, as well as some Varnish-specific ones.
  See the documentation for further information.  `Bug #712`_ and `bug #485`_

.. _bug #712: http://varnish-cache.org/trac/ticket/712
.. _bug #485: http://varnish-cache.org/trac/ticket/485

varnishtest
***********

- add ``-l`` and ``-L`` switches which leave ``/tmp/vtc.*`` behind on
  error and unconditionally respectively.
- add ``-j`` parameter to run tests in parallell and use this by
  default.

varnishtop
**********

- add ``-p $period`` parameter.  The units in varnishtop were
  previously undefined, they are now in requests/period.  The default
  period is 60 seconds.

varnishlog
**********

- group requests by default.  This can be turned off by using ``-O``
- the ``-o`` parameter is now a no-op and is ignored.

VMODs
-----

- Add a std vmod which includes a random function, log, syslog,
  fileread, collect,

VCL
---

- Change string concatenation to be done using ``+`` rather than
  implicitly.
- Stop using ``%xx`` escapes in VCL strings.
- Change ``req.hash += value`` to ``hash_data(value)``
- Variables in VCL now have distinct read/write access
- ``bereq.connect_timeout`` is now available in ``vcl_pipe``.
- Make it possible to declare probes outside of a director. Please see
  the documentation on how to do this.
- The VCL compiler has been reworked greatly, expanding its abilities
  with regards to what kinds of expressions it understands.
- Add ``beresp.backend.name``, ``beresp.backend.ip`` and
  ``beresp.backend.port`` variables.  They are only available from
  ``vcl_fetch`` and are read only.  `Bug #481`_
- The default VCL now calls pass for any objects where
  ``beresp.http.Vary == "*"``.  `Bug #787`_
- The ``log`` keyword has been moved to the ``std`` vmod.
- It is now possible to choose which storage backend to be used
- Add variables ``storage.$name.free_space``,
  ``storage.$name.used_space`` and ``storage.$name.happy``
- The variable ``req.can_gzip`` tells us whether the client accepts
  gzipped objects or not.
- ``purge`` is now called ``ban``, since that is what it really is and
  has always been.
- ``req.esi_level`` is now available.  `Bug #782`_
- esi handling is now controlled by the ``beresp.do_esi`` boolean rather
  than the ``esi`` function.
- ``beresp.do_gzip`` and ``beresp.do_gunzip`` now control whether an
  uncompressed object should be compressed and a compressed object
  should be uncompressed in the cache.
- make it possible to control compression level using the
  ``gzip_level`` parameter.
- ``obj.cacheable`` and ``beresp.cacheable`` have been removed.
  Cacheability is now solely through the ``beresp.ttl`` and
  ``beresp.grace`` variables.
- setting the ``obj.ttl`` or ``beresp.ttl`` to zero now also sets the
  corresponding grace to zero.  If you want a non-zero grace, set
  grace after setting the TTL.
- ``return(pass)`` in ``vcl_fetch`` has been renamed to
  ``return(hit_for_pass)`` to make it clear that pass in ``vcl_fetch``
  and ``vcl_recv`` are different beasts.
- Add actual purge support.  Doing ``purge`` will remove an object and
  all its variants.

.. _bug #481: http://varnish-cache.org/trac/ticket/481
.. _bug #787: http://varnish-cache.org/trac/ticket/787
.. _bug #782: http://varnish-cache.org/trac/ticket/782


Libraries
---------

- ``libvarnishapi`` has been overhauled and the API has been broken.
  Please see git commit logs and the support tools to understand
  what's been changed.
- Add functions to walk over all the available counters.  This is
  needed because some of the counter names might only be available at
  runtime.
- Limit the amount of time varnishapi waits for a shared memory log
  to appear before returning an error.
- All libraries but ``libvarnishapi`` have been moved to a private
  directory as they are not for public consumption and have no ABI/API
  guarantees.

Other
-----

- Python is now required to build
- Varnish Cache is now consistently named Varnish Cache.
- The compilation process now looks for kqueue on NetBSD
- Make it possible to use a system jemalloc rather than the bundled
  version.
- The documentation has been improved all over and should now be in
  much better shape than before

===========================
Changes from 2.1.4 to 2.1.5
===========================

varnishd
--------

-  On pass from vcl\_recv, we did not remove the backends Content-Length
   header before adding our own. This could cause confusion for browsers
   and has been fixed.

-  Make pass with content-length work again. An issue with regards to
   304, Content-Length and pass has been resolved.

-  An issue relating to passed requests with If-Modified-Since headers
   has been fixed. Varnish did not recognize that the 304-response did
   not have a body.

-  A potential lock-inversion with the ban lurker thread has been
   resolved.

-  Several build-dependency issues relating to rst2man have been fixed.
   Varnish should now build from source without rst2man if you are using
   tar-balls.

-  Ensure Varnish reads the expected last CRLF after chunked data from
   the backend. This allows re-use of the connection.

-  Remove a GNU Make-ism during make dist to make BSD happier.

-  Document the log, set, unset, return and restart statements in the
   VCL documentation.

-  Fix an embarrassingly old bug where Varnish would run out of
   workspace when requests come in fast over a single connection,
   typically during synthetic benchmarks.

-  Varnish will now allow If-Modified-Since requests to objects without
   a Last-Modified-header, and instead use the time the object was
   cached instead.

-  Do not filter out Content-Range headers in pass.

-  Require -d, -b, -f, -S or -T when starting Varnishd. In human terms,
   this means that it is legal to start varnishd without a Vcl or
   backend, but only if you have a CLI channel of some kind.

-  Don't suppress Cache-Control headers in pass responses.

-  Merge multi-line Cache-Control and Vary header fields. Until now, no
   browsers have needed this, but Chromium seems to find it necessary to
   spread its Cache-Control across two lines, and we get to deal with
   it.

-  Make new-purge not touch busy objects. This fixes a potential crash
   when calling VRT\_purge.

-  If there are everal grace-able objects, pick the least expired one.

-  Fix an issue with varnishadm -T :6082 shorthand.

-  Add bourn-shell like "here" documents on the CLI. Typical usage:
   vcl.inline vcl\_new << 42 backend foo {...} sub vcl\_recv {...} 42

-  Add CLI version to the CLI-banner, starting with version 1.0 to mark
   here-documents.

-  Fix a problem with the expiry thread slacking off during high load.

varnishtest
-----------

-  Remove no longer existing -L option.

===========================
Changes from 2.1.3 to 2.1.4
===========================

varnishd
--------

-  An embarrasing typo in the new binary heap layout caused inflated
   obj/objcore/objhdr counts and could cause odd problems when the LRU
   expunge mechanism was invoked. This has been fixed.

-  We now have updated documentation in the reStructuredText format.
   Manual pages and reference documentation are both built from this.

-  We now include a DNS director which uses DNS for choosing which
   backend to route requests to. Please see the documentation for more
   details.

-  If you restarted a request, the HTTP header X-Forwarded-For would be
   updated multiple times. This has been fixed.

-  If a VCL contained a % sign, and the vcl.show CLI command was used,
   varnishd would crash. This has been fixed.

-  When doing a pass operation, we would remove the Content-Length, Age
   and Proxy-Auth headers. We are no longer doing this.

-  now has a string representation, making it easier to construct
   Expires headers in VCL.

-  In a high traffic environment, we would sometimes reuse a file
   descriptor before flushing the logs from a worker thread to the
   shared log buffer. This would cause confusion in some of the tools.
   This has been fixed by explicitly flushing the log when a backend
   connection is closed.

-  If the communication between the management and the child process
   gets out of sync, we have no way to recover. Previously, varnishd
   would be confused, but we now just kill the child and restart it.

-  If the backend closes the connection on us just as we sent a request
   to it, we retry the request. This should solve some interoperability
   problems with Apache and the mpm-itk multi processing module.

-  varnishd now only provides help output the current CLI session is
   authenticated for.

-  If the backend does not tell us which length indication it is using,
   we now assume the resource ends EOF at.

-  The client director now has a variable client.identity which is used
   to choose which backend should receive a given request.

-  The Solaris port waiter has been updated, and other portability fixes
   for Solaris.

-  There was a corner case in the close-down processing of pipes, this
   has now been fixed.

-  Previously, if we stopped polling a backend which was sick, it never
   got marked as healthy. This has now been changed.

-  It is now possible to specify ports as part of the .host field in
   VCL.

-  The synthetic counters were not locked properly, and so the sms\_
   counters could underflow. This has now been fixed.

-  The value of obj.status as a string in vcl\_error would not be
   correct in all cases. This has been fixed.

-  Varnish would try to trim storage segments completely filled when
   using the malloc stevedore and the object was received chunked
   encoding. This has been fixed.

-  If a buggy backend sends us a Vary header with two colons, we would
   previously abort. We now rather fix this up and ignore the extra
   colon.

-  req.hash\_always\_miss and req.hash\_ignore\_busy has been added, to
   make preloading or periodically refreshing content work better.

varnishncsa
-----------

-  varnishncsa would in some cases be confused by ESI requests and
   output invalid lines. This has now been fixed.

varnishlog
----------

-  varnishlog now allows -o and -u together.

varnishtop
----------

-  varnishtop would crash on 32 bit architectures. This has been fixed.

libvarnishapi
-------------

-  Regex inclusion and exclusion had problems with matching particular
   parts of the string being matched. This has been fixed.


===========================
Changes from 2.1.2 to 2.1.3
===========================

varnishd
--------

-  Improve scalability of critbit.

-  The critbit hash algorithm has now been tightened to make sure the
   tree is in a consistent state at all points, and the time we wait for
   an object to cool off after it is eligible for garbage collection has
   been tweaked.

-  Add log command to VCL. This emits a VCL\_log entry into the shared
   memory log.

-  Only emit Length and ReqEnd log entries if we actually have an XID.
   This should get rid of some empty log lines in varnishncsa.

-  Destroy directors in a predictable fashion, namely reverse of
   creation order.

-  Fix bug when ESI elements spanned storage elements causing a panic.

-  In some cases, the VCL compiler would panic instead of giving
   sensible messages. This has now been fixed.

-  Correct an off-by-one error when the requested range exceeds the size
   of an object.

-  Handle requests for the end of an object correctly.

-  Allow tabulator characters in the third field of the first line of
   HTTP requests

-  On Solaris, if the remote end sends us an RST, all system calls
   related to that socket will return EINVAL. We now handle this better.

libvarnishapi
-------------

-  The -X parameter didn't work correctly. This has been fixed.

===========================
Changes from 2.1.1 to 2.1.2
===========================

varnishd
--------

-  When adding Range support for 2.1.1, we accidentially introduced a
   bug which would append garbage to objects larger than the chunk size,
   by default 128k. Browsers would do the right thing due to
   Content-Length, but some load balancers would get very confused.

===========================
Changes from 2.1.1 to 2.1.1
===========================

varnishd
--------

-  The changelog in 2.1.0 included syntax errors, causing the generated
   changelog to be empty.

-  The help text for default\_grace was wrongly formatted and included a
   syntax error. This has now been fixed.

-  varnishd now closes the file descriptor used to read the management
   secret file (from the -S parameter).

-  The child would previously try to close every valid file descriptor,
   something which could cause problems if the file descriptor ulimit
   was set too high. We now keep track of all the file descriptors we
   open and only close up to that number.

-  ESI was partially broken in 2.1.0 due to a bug in the rollback of
   session workspace. This has been fixed.

-  Reject the authcommand rather than crash if there is no -S parameter
   given.

-  Align pointers in allocated objects. This will in theory make Varnish
   a tiny bit faster at the expense of slightly more memory usage.

-  Ensure the master process process id is updated in the shared memory
   log file after we go into the background.

-  HEAD requests would be converted to GET requests too early, which
   affected pass and pipe. This has been fixed.

-  Update the documentation to point out that the TTL is no longer taken
   into account to decide whether an object is cacheable or not.

-  Add support for completely obliterating an object and all variants of
   it. Currently, this has to be done using inline C.

-  Add experimental support for the Range header. This has to be enabled
   using the parameter http\_range\_support.

-  The critbit hasher could get into a deadlock and had a race
   condition. Both those have now been fixed.

varnishsizes
-----------~

-  varnishsizes, which is like varnishhost, but for the length of
   objects, has been added..


===========================
Changes from 2.0.6 to 2.1.0
===========================

varnishd
--------

-  Persistent storage is now experimentally supported using the
   persistent stevedore. It has the same command line arguments as the
   file stevedore.

-  obj.\* is now called beresp.\* in vcl\_fetch, and obj.\* is now
   read-only.

-  The regular expression engine is now PCRE instead of POSIX regular
   expressions.

-  req.\* is now available in vcl\_deliver.

-  Add saint mode where we can attempt to grace an object if we don't
   like the backend response for some reason.

   Related, add saintmode\_threshold which is the threshold for the
   number of objects to be added to the trouble list before the backend
   is considered sick.

-  Add a new hashing method called critbit. This autoscales and should
   work better on large object workloads than the classic hash. Critbit
   has been made the default hash algorithm.

-  When closing connections, we experimented with sending RST to free up
   load balancers and free up threads more quickly. This caused some
   problems with NAT routers and so has been reverted for now.

-  Add thread that checks objects against ban list in order to prevent
   ban list from growing forever. Note that this needs purges to be
   written so they don't depend on req.\*. Enabled by setting
   ban\_lurker\_sleep to a nonzero value.

-  The shared memory log file format was limited to maximum 64k
   simultaneous connections. This is now a 32 bit field which removes
   this limitation.

-  Remove obj\_workspace, this is now sized automatically.

-  Rename acceptors to waiters

-  vcl\_prefetch has been removed. It was never fully implemented.

-  Add support for authenticating CLI connections.

-  Add hash director that chooses which backend to use depending on
   req.hash.

-  Add client director that chooses which backend to use depending on
   the client's IP address. Note that this ignores the X-Forwarded-For
   header.

-  varnishd now displays a banner by default when you connect to the
   CLI.

-  Increase performance somewhat by moving statistics gathering into a
   per-worker structure that is regularly flushed to the global stats.

-  Make sure we store the header and body of object together. This may
   in some cases improve performance and is needed for persistence.

-  Remove client-side address accounting. It was never used for anything
   and presented a performance problem.

-  Add a timestamp to bans, so you can know how old they are.

-  Quite a few people got confused over the warning about not being able
   to lock the shared memory log into RAM, so stop warning about that.

-  Change the default CLI timeout to 10 seconds.

-  We previously forced all inserts into the cache to be GET requests.
   This has been changed to allow POST as well in order to be able to
   implement purge-on-POST semantics.

-  The CLI command stats now only lists non-zero values.

-  The CLI command stats now only lists non-zero values.

-  Use daemon(3) from libcompat on Darwin.

-  Remove vcl\_discard as it causes too much complexity and never
   actually worked particularly well.

-  Remove vcl\_timeout as it causes too much complexity and never
   actually worked particularly well.

-  Update the documentation so it refers to sess\_workspace, not
   http\_workspace.

-  Document the -i switch to varnishd as well as the server.identity and
   server.hostname VCL variables.

-  purge.hash is now deprecated and no longer shown in help listings.

-  When processing ESI, replace the five mandatory XML entities when we
   encounter them.

-  Add string representations of time and relative time.

-  Add locking for n\_vbe\_conn to make it stop underflowing.

-  When ESI-processing content, check for illegal XML character
   entities.

-  Varnish can now connect its CLI to a remote instance when starting
   up, rather than just being connected to.

-  It is no longer needed to specify the maximum number of HTTP headers
   to allow from backends. This is now a run-time parameter.

-  The X-Forwarded-For header is now generated by vcl\_recv rather than
   the C code.

-  It is now possible to not send all CLI traffic to syslog.

-  It is now possible to not send all CLI traffic to syslog.

-  In the case of varnish crashing, it now outputs a identifying string
   with the OS, OS revision, architecture and storage parameters
   together with the backtrace.

-  Use exponential backoff when we run out of file descriptors or
   sessions.

-  Allow setting backend timeouts to zero.

-  Count uptime in the shared memory log.

-  Try to detect the case of two running varnishes with the same shmlog
   and storage by writing the master and child process ids to the shmlog
   and refusing to start if they are still running.

-  Make sure to use EOF mode when serving ESI content to HTTP/1.0
   clients.

-  Make sure we close the connection if it either sends Connection:
   close or it is a HTTP/1.0 backend that does not send Connection:
   keep-alive.

-  Increase the default session workspace to 64k on 64-bit systems.

-  Make the epoll waiter use level triggering, not edge triggering as
   edge triggering caused problems on very busy servers.

-  Handle unforeseen client disconnections better on Solaris.

-  Make session lingering apply to new sessions, not just reused
   sessions.

varnishstat
-----------

-  Make use of the new uptime field in the shared memory log rather than
   synthesizing it from the start time.

varnishlog
----------

-  Exit at the end of the file when started with -d.

varnishadm
----------

-  varnishadm can now have a timeout when trying to connect to the
   running varnishd.

-  varnishadm now knows how to respond to the secret from a secured
   varnishd

===========================
Changes from 2.0.5 to 2.0.6
===========================

varnishd
--------

-  2.0.5 had an off-by-one error in the ESI handling causing includes to
   fail a large part of the time. This has now been fixed.

-  Try harder to not confuse backends when sending them backend probes.
   We half-closed the connection, something some backends thought meant
   we had dropped the connection. Stop doing so, and add the capability
   for specifying the expected response code.

-  In 2.0.5, session lingering was turned on. This caused statistics to
   not be counted often enough in some cases. This has now been fixed.

-  Avoid triggering an assert if the other end closes the connection
   while we are lingering and waiting for another request from them.

-  When generating backtraces, prefer the built-in backtrace function if
   such exists. This fixes a problem compiling 2.0.5 on Solaris.

-  Make it possible to specify the per-thread stack size. This might be
   useful on 32 bit systems with their limited address space.

-  Document the -C option to varnishd.

===========================
Changes from 2.0.4 to 2.0.5
===========================

varnishd
--------

-  Handle object workspace overruns better.

-  Allow turning off ESI processing per request by using set req.esi =
   off.

-  Tell the kernel that we expect to use the mmap-ed file in a random
   fashion. On Linux, this turns off/down readahead and increases
   performance.

-  Make it possible to change the maximum number of HTTP headers we
   allow by passing --with-max-header-fields=NUM rather than changing
   the code.

-  Implement support for HTTP continuation lines.

-  Change how connections are closed and only use SO\_LINGER for orderly
   connection closure. This should hopefully make worker threads less
   prone to hangups on network problems.

-  Handle multi-element purges correctly. Previously we ended up with
   parse errors when this was done from VCL.

-  Handle illegal responses from the backend better by serving a 503
   page rather than panic-ing.

-  When we run into an assertion that is not true, Varnish would
   previously dump a little bit of information about itself. Extend that
   information with a backtrace. Note that this relies on the varnish
   binary being unstripped.

-  Add a session\_max parameter that limits the maximum number of
   sessions we keep open before we start dropping new connections
   summarily.

-  Try to consume less memory when doing ESI processing by properly
   rolling back used workspace after processing an object. This should
   make it possible to turn sess\_workspace quite a bit for users with
   ESI-heavy pages.

-  Turn on session\_linger by default. Tests have shown that
   session\_linger helps a fair bit with performance.

-  Rewrite the epoll acceptor for better performance. This should lead
   to both higher processing rates and maximum number of connections on
   Linux.

-  Add If-None-Match support, this gives significant bandwidth savings
   for users with compliant browsers.

-  RFC2616 specifies that ETag, Content-Location, Expires, Cache-Control
   and Vary should be emitted when delivering a response with the 304
   response code.

-  Various fixes which makes Varnish compile and work on AIX.

-  Turn on TCP\_DEFER\_ACCEPT on Linux. This should make us less
   suspecible to denial of service attacks as well as give us slightly
   better performance.

-  Add an .initial property to the backend probe specification. This is
   the number of good probes we pretend to have seen. The default is one
   less than .threshold, which means the first probe will decide if we
   consider the backend healthy.

-  Make it possible to compare strings against other string-like
   objects, not just plain strings. This allows you to compare two
   headers, for instance.

-  When support for restart in vcl\_error was added, there was no check
   to prevent infinte recursion. This has now been fixed.

-  Turn on purge\_dups by default. This should make us consume less
   memory when there are many bans for the same pattern added.

-  Add a new log tag called FetchError which tries to explain why we
   could not fetch an object from the backend.

-  Change the default srcaddr\_ttl to 0. It is not used by anything and
   has been removed in the development version. This will increase
   performance somewhat.

varnishtop
----------

-  varnishtop did not handle variable-length log fields correctly. This
   is now fixed.

-  varnishtop previously did not print the name of the tag, which made
   it very hard to understand. We now print out the tag name.

===========================
Changes from 2.0.3 to 2.0.4
===========================

varnishd
--------

-  Make Varnish more portable by pulling in fixes for Solaris and
   NetBSD.

-  Correct description of -a in the manual page.

-  Ensure we are compiling in C99 mode.

-  If error was called with a null reason, we would crash on Solaris.
   Make sure this no longer happens.

-  Varnish used to crash if you asked it to use a non-existent waiter.
   This has now been fixed.

-  Add documentation to the default VCL explaining that using
   Connection: close in vcl\_close is generally a good idea.

-  Add minimal facility for dealing with TELNET option negotiation by
   returning WONT to DO and DONT requests.

-  If the backend is unhealthy, use a graced object if one is available.

-  Make server.hostname and server.identity available to VCL. The latter
   can be set with the -i parameter to varnishd.

-  Make restart available from vcl\_error.

-  Previously, only the TTL of an object was considered in whether it
   would be marked as cacheable. This has been changed to take the grace
   into consideration as well.

-  Previously, if an included ESI fragment had a zero size, we would
   send out a zero-sized chunk which signifies end-of-transmission. We
   now ignore zero-sized chunks.

-  We accidentially slept for far too long when we reached the maximum
   number of open file descriptors. This has been corrected and
   accept\_fd\_holdoff now works correctly.

-  Previously, when ESI processing, we did not look at the full length,
   but stopped at the first NULL byte. We no longer do that, enabling
   ESI processing of binary data.

varnishtest
-----------

-  Make sure system "..." returns successfully to ensure test failures
   do not go unnoticed.

-  Make it possible to send NULL bytes through the testing framework.

===========================
Changes from 2.0.2 to 2.0.3
===========================

varnishd
--------

-  Handle If-Modified-Since and ESI sub-objects better, fixing a problem
   where we sometimes neglected to insert included objects.

-  restart in vcl\_hit is now supported.

-  Setting the TTL of an object to 0 seconds would sometimes cause it to
   be delivered for up to one second - epsilon. This has been corrected
   and we should now never deliver those objects to other clients.

-  The malloc storage backend now prints the maximum storage size, just
   like the file backend.

-  Various small documentation bugs have been fixed.

-  Varnish did not set a default interval for backend probes, causing it
   to poll the backend continuously. This has been corrected.

-  Allow "true" and "false" when setting boolean parameters, in addition
   to on/off, enable/disable and yes/no.

-  Default to always talking HTTP 1.1 with the backend.

-  Varnish did not make sure the file it was loading was a regular file.
   This could cause Varnish to crash if it was asked to load a directory
   or other non-regular file. We now check that the file is a regular
   file before loading it.

-  The binary heap used for expiry processing had scalability problems.
   Work around this by using stripes of a fixed size, which should make
   this scale better, particularly when starting up and having lots of
   objects.

-  When we imported the jemalloc library into the Varnish tree, it did
   not compile without warnings. This has now been fixed.

-  Varnish took a very long time to detect that the backend did not
   respond. To remedy this, we now have read timeouts in addition to the
   connect timeout. Both the first\_byte\_timeout and the
   between\_bytes\_timeout defaults to 60 seconds. The connect timeout
   is no longer in milliseconds, but rather in seconds.

-  Previously, the VCL to C conversion as well as the invocation of the
   C compiler was done in the management process. This is now done in a
   separate sub-process. This prevents any bugs in the VCL compiler from
   affecting the management process.

-  Chunked encoding headers were counted in the statistics for header
   bytes. They no longer are.

-  ESI processed objects were not counted in the statistics for body
   bytes. They now are.

-  It is now possible to adjust the maximum record length of log entries
   in the shmlog by tuning the shm\_reclen parameter.

-  The management parameters listed in the CLI were not sorted, which
   made it hard to find the parameter you were looking for. They are now
   sorted, which should make this easier.

-  Add a new hashing type, "critbit", which uses a lock-less tree based
   lookup algorithm. This is experimental and should not be enabled in
   production environments without proper testing.

-  The session workspace had a default size of 8k. It is now 16k, which
   should make VCLs where many headers are processed less prone to
   panics.

-  We have seen that people seem to be confused as to which actions in
   the different VCL functions return and which ones don't. Add a new
   syntax return(action) to make this more explicit. The old syntax is
   still supported.

-  Varnish would return an error if any of the management IPs listed in
   the -T parameter could not be listened to. We now only return an
   error if none of them can be listened to.

-  In the case of the backend or client giving us too many parameters,
   we used to just ignore the overflowing headers. This is problematic
   if you end up ignoreing Content-Length, Transfer-Encoding and similar
   headers. We now give out a 400 error to the client if it sends us too
   many and 503 if we get too many from the backend.

-  We used panic if we got a too large chunked header. This behaviour
   has been changed into just failing the transaction.

-  Varnish now supports an extended purge method where it is possible to
   do purge req.http.host ~ "web1.com" && req.url ~ "\\.png" and
   similar. See the documentation for details.

-  Under heavy load, Varnish would sometimes crash when trying to update
   the per-request statistics. This has now been fixed.

-  It is now possible to not save the hash string in the session and
   object workspace. This will save a lot of memory on sites with many
   small objects. Disabling the purge\_hash parameter also disables the
   purge.hash facility.

-  Varnish now supports !~ as a "no match" regular expression matcher.

-  In some cases, you could get serialised access to "pass" objects. We
   now make it default to the default\_ttl value; this can be overridden
   in vcl\_fetch.

-  Varnish did not check the syntax of regsub calls properly. More
   checking has been added.

-  If the client closed the connection while Varnish was processing ESI
   elements, Varnish would crash while trying to write the object to the
   client. We now check if the client has closed the connection.

-  The ESI parser had a bug where it would crash if an XML comment would
   span storage segments. This has been fixed.

VCL Manual page
--------------~

-  The documentation on how capturing parentheses work was wrong. This
   has been corrected.

-  Grace has now been documented.

varnishreplay
-------------

-  varnishreplay did not work correctly on Linux, due to a too small
   stack. This has now been fixed.

===========================
Changes from 2.0.1 to 2.0.2
===========================

varnishd
--------

-  In high-load situations, when using ESI, varnishd would sometimes
   mishandle objects and crash. This has been worked around.

varnishreplay
-------------

-  varnishreplay did not work correctly on Linux, due to a too small
   stack. This has now been fixed.


=========================
Changes from 2.0 to 2.0.1
=========================

varnishd
--------

-  When receiving a garbled HTTP request, varnishd would sometimes
   crash. This has been fixed.

-  There was an off-by-one error in the ACL compilation. Now fixed.

Red Hat spec file
----------------~

-  A typo in the spec file made the .rpm file names wrong.

=========================
Changes from 1.1.2 to 2.0
=========================

varnishd
--------

-  Only look for sendfile on platforms where we know how to use it,
   which is FreeBSD for now.

-  Make it possible to adjust the shared memory log size and bump the
   size from 8MB to 80MB.

-  Fix up the handling of request bodies to better match what RFC2616
   mandates. This makes PUT, DELETE, OPTIONS and TRACE work in addition
   to POST.

-  Change how backends are defined, to a constant structural defintion
   style. See http://varnish.projects.linpro.no/wiki/VclSyntaxChanges
   for the details.

-  Add directors, which wrap backends. Currently, there's a random
   director and a round-robin director.

-  Add "grace", which is for how long and object will be served, even
   after it has expired. To use this, both the object's and the
   request's grace parameter need to be set.

-  Manual pages have been updated for new VCL syntax and varnishd
   options.

-  Man pages and other docs have been updated.

-  The shared memory log file is now locked in memory, so it should not
   be paged out to disk.

-  We now handle Vary correctly, as well as Expect.

-  ESI include support is implemented.

-  Make it possible to limit how much memory the malloc uses.

-  Solaris is now supported.

-  There is now a regsuball function, which works like regsub except it
   replaces all occurences of the regex, not just the first.

-  Backend and director declarations can have a .connect\_timeout
   parameter, which tells us how long to wait for a successful
   connection.

-  It is now possible to select the acceptor to use by changing the
   acceptor parameter.

-  Backends can have probes associated with them, which can be checked
   with req.backend.health in VCL as well as being handled by directors
   which do load-balancing.

-  Support larger-than-2GB files also on 32 bit hosts. Please note that
   this does not mean we can support caches bigger than 2GB, it just
   means logfiles and similar can be bigger.

-  In some cases, we would remove the wrong header when we were
   stripping Content-Transfer-Encoding headers from a request. This has
   been fixed.

-  Backends can have a .max\_connections associated with them.

-  On Linux, we need to set the dumpable bit on the child if we want
   core dumps. Make sure it's set.

-  Doing purge.hash() with an empty string would cause us to dump core.
   Fixed so we don't do that any more.

-  We ran into a problem with glibc's malloc on Linux where it seemed
   like it failed to ever give memory back to the OS, causing the system
   to swap. We have now switched to jemalloc which appears not to have
   this problem.

-  max\_restarts was never checked, so we always ended up running out of
   workspace. Now, vcl\_error is called when we reach max\_restarts.

varnishtest
-----------

-  varnishtest is a tool to do correctness tests of varnishd. The test
   suite is run by using make check.

varnishtop
----------

-  We now set the field widths dynamically based on the size of the
   terminal and the name of the longest field.

varnishstat
-----------

-  varnishstat -1 now displays the uptime too.

varnishncsa
-----------

-  varnishncsa now does fflush after each write. This makes tail -f work
   correctly, as well as avoiding broken lines in the log file.

-  It is possible to get varnishncsa to output the X-Forwarded-For
   instead of the client IP by passing -f to it.

Build system
-----------~

-  Various sanity checks have been added to configure, it now complains
   about no ncurses or if SO\_RCVTIMEO or SO\_SNDTIMEO are
   non-functional. It also aborts if there's no working acceptor
   mechanism

-  The C compiler invocation is decided by the configure script and can
   now be overridden by passing VCC\_CC when running configure.

===========================
Changes from 1.1.1 to 1.1.2
===========================

varnishd
--------

-  When switching to a new VCL configuration, a race condition exists
   which may cause Varnish to reference a backend which no longer exists
   (see `ticket #144 <http://varnish.projects.linpro.no/ticket/144>`_).
   This race condition has not been entirely eliminated, but it should
   occur less frequently.

-  When dropping a TCP session before any requests were processed, an
   assertion would be triggered due to an uninitialized timestamp (see
   `ticket #132 <http://varnish.projects.linpro.no/ticket/132>`_). The
   timestamp is now correctly initialized.

-  Varnish will now correctly generate a Date: header for every response
   instead of copying the one it got from the backend (see `ticket
   #157 <http://varnish.projects.linpro.no/ticket/157>`_).

-  Comparisons in VCL which involve a non-existent string (usually a
   header which is not present in the request or object being processed)
   would cause a NULL pointer dereference; now the comparison will
   simply fail.

-  A bug in the VCL compiler which would cause a double-free when
   processing include directives has been fixed.

-  A resource leak in the worker thread management code has been fixed.

-  When connecting to a backend, Varnish will usually get the address
   from a cache. When the cache is refreshed, existing connections may
   end up with a reference to an address structure which no longer
   exists, resulting in a crash. This race condition has been somewhat
   mitigated, but not entirely eliminated (see `ticket
   #144 <http://varnish.projects.linpro.no/ticket/144>`_.)

-  Varnish will now pass the correct protocol version in pipe mode: the
   backend will get what the client sent, and vice versa.

-  The core of the pipe mode code has been rewritten to increase
   robustness and eliminate spurious error messages when either end
   closes the connection in a manner Varnish did not anticipate.

-  A memory leak in the backend code has been plugged.

-  When using the kqueue acceptor, if a client shuts down the request
   side of the connection (as many clients do after sending their final
   request), it was possible for the acceptor code to receive the EOF
   event and recycle the session while the last request was still being
   serviced, resulting in a assertion failure and a crash when the
   worker thread later tried to delete the session. This should no
   longer happen (see `ticket
   #162 <http://varnish.projects.linpro.no/ticket/162>`_.)

-  A mismatch between the recorded length of a cached object and the
   amount of data actually present in cache for that object can
   occasionally occur (see `ticket
   #167 <http://varnish.projects.linpro.no/ticket/167>`_.) This has been
   partially fixed, but may still occur for error pages generated by
   Varnish when a problem arises while retrieving an object from the
   backend.

-  Some socket-related system calls may return unexpected error codes
   when operating on a TCP connection that has been shut down at the
   other end. These error codes would previously cause assertion
   failures, but are now recognized as harmless conditions.

varnishhist
-----------

-  Pressing 0 though 9 while varnishhist is running will change the
   refresh interval to the corresponding power of two, in seconds.

varnishncsa
-----------

-  The varnishncsa tool can now daemonize and write a PID file like
   varnishlog, using the same command-line options. It will also reopen
   its output upon receipt of a SIGHUP if invoked with -w.

varnishstat
-----------

-  Pressing 0 though 9 while varnishstat is running will change the
   refresh interval to the corresponding power of two, in seconds.

Build system
-----------~

-  Varnish's <queue.h> has been modified to avoid conflicts with
   <sys/queue.h> on platforms where the latter is included indirectly
   through system headers.

-  Several steps have been taken towards Solaris support, but this is
   not yet complete.

-  When configure was run without an explicit prefix, Varnish's idea of
   the default state directory would be garbage and a state directory
   would have to be specified manually with -n. This has been corrected.

=========================
Changes from 1.1 to 1.1.1
=========================

varnishd
--------

-  The code required to allow VCL to read obj.status, which had
   accidentally been left out, has now been added.

-  Varnish will now always include a Connection: header in its reply to
   the client, to avoid possible misunderstandings.

-  A bug that triggered an assertion failure when generating synthetic
   error documents has been corrected.

-  A new VCL function, purge\_url, provides the same functionality as
   the url.purge management command.

-  Previously, Varnish assumed that the response body should be sent
   only if the request method was GET. This was a problem for custom
   request methods (such as PURGE), so the logic has been changed to
   always send the response body except in the specific case of a HEAD
   request.

-  Changes to run-time parameters are now correctly propagated to the
   child process.

-  Due to the way run-time parameters are initialized at startup,
   varnishd previously required the nobody user and the nogroup group to
   exist even if a different user and group were specified on the
   command line. This has been corrected.

-  Under certain conditions, the VCL compiler would carry on after a
   syntax error instead of exiting after reporting the error. This has
   been corrected.

-  The manner in which the hash string is assembled has been modified to
   reduce memory usage and memory-to-memory copying.

-  Before calling vcl\_miss, Varnish assembles a tentative request
   object for the backend request which will usually follow. This object
   would be leaked if vcl\_miss returned anything else than fetch. This
   has been corrected.

-  The code necessary to handle an error return from vcl\_fetch and
   vcl\_deliver had inadvertantly been left out. This has been
   corrected.

-  Varnish no longer prints a spurious "child died" message (the result
   of reaping the compiler process) after compiling a new VCL
   configuration.

-  Under some circumstances, due to an error in the workspace management
   code, Varnish would lose the "tail" of a request, i.e. the part of
   the request that has been received from the client but not yet
   processed. The most obvious symptom of this was that POST requests
   would work with some browsers but not others, depending on details of
   the browser's HTTP implementation. This has been corrected.

-  On some platforms, due to incorrect assumptions in the CLI code, the
   management process would crash while processing commands received
   over the management port. This has been corrected.

Build system
-----------~

-  The top-level Makefile will now honor $DESTDIR when creating the
   state directory.

-  The Debian and RedHat packages are now split into three (main / lib /
   devel) as is customary.

-  A number of compile-time and run-time portability issues have been
   addressed.

-  The autogen.sh script had workarounds for problems with the GNU
   autotools on FreeBSD; these are no longer needed and have been
   removed.

-  The libcompat library has been renamed to libvarnishcompat and is now
   dynamic rather than static. This simplifies the build process and
   resolves an issue with the Mac OS X linker.

=========================
Changes from 1.0.4 to 1.1
=========================

varnishd
--------

-  Readability of the C source code generated from VCL code has been
   improved.

-  Equality (==) and inequality (!=) operators have been implemented for
   IP addresses (which previously could only be compared using ACLs).

-  The address of the listening socket on which the client connection
   was received is now available to VCL as the server.ip variable.

-  Each object's hash key is now computed based on a string which is
   available to VCL as req.hash. A VCL hook named vcl\_hash has been
   added to allow VCL scripts to control hash generation (for instance,
   whether or not to include the value of the Host: header in the hash).

-  The setup code for listening sockets has been modified to detect and
   handle situations where a host name resolves to multiple IP
   addresses. It will now attempt to bind to each IP address separately,
   and report a failure only if none of them worked.

-  Network or protocol errors that occur while retrieving an object from
   a backend server now result in a synthetic error page being inserted
   into the cache with a 30-second TTL. This should help avoid driving
   an overburdened backend server into the ground by repeatedly
   requesting the same object.

-  The child process will now drop root privileges immediately upon
   startup. The user and group to use are specified with the user and
   group run-time parameters, which default to nobody and nogroup,
   respectively. Other changes have been made in an effort to increase
   the isolation between parent and child, and reduce the impact of a
   compromise of the child process.

-  Objects which are received from the backend with a Vary: header are
   now stored separately according to the values of the headers
   specified in Vary:. This allows Varnish to correctly cache e.g.
   compressed and uncompressed versions of the same object.

-  Each Varnish instance now has a name, which by default is the host
   name of the machine it runs on, but can be any string that would be
   valid as a relative or absolute directory name. It is used to
   construct the name of a directory in which the server state as well
   as all temporary files are stored. This makes it possible to run
   multiple Varnish instances on the same machine without conflict.

-  When invoked with the -C option, varnishd will now not just translate
   the VCL code to C, but also compile the C code and attempt to load
   the resulting shared object.

-  Attempts by VCL code to reference a variable outside its scope or to
   assign a value to a read-only variable will now result in
   compile-time rather than run-time errors.

-  The new command-line option -F will make varnishd run in the
   foreground, without enabling debugging.

-  New VCL variables have been introduced to allow inspection and
   manipulation of the request sent to the backend (bereq.request,
   bereq.url, bereq.proto and bereq.http) and the response to the client
   (resp.proto, resp.status, resp.response and resp.http).

-  Statistics from the storage code (including the amount of data and
   free space in the cache) are now available to varnishstat and other
   statistics-gathering tools.

-  Objects are now kept on an LRU list which is kept loosely up-to-date
   (to within a few seconds). When cache runs out, the objects at the
   tail end of the LRU list are discarded one by one until there is
   enough space for the freshly requested object(s). A VCL hook,
   vcl\_discard, is allowed to inspect each object and determine its
   fate by returning either keep or discard.

-  A new VCL hook, vcl\_deliver, provides a chance to adjust the
   response before it is sent to the client.

-  A new management command, vcl.show, displays the VCL source code of
   any loaded configuration.

-  A new VCL variable, now, provides VCL scripts with the current time
   in seconds since the epoch.

-  A new VCL variable, obj.lastuse, reflects the time in seconds since
   the object in question was last used.

-  VCL scripts can now add an HTTP header (or modify the value of an
   existing one) by assigning a value to the corresponding variable, and
   strip an HTTP header by using the remove keyword.

-  VCL scripts can now modify the HTTP status code of cached objects
   (obj.status) and responses (resp.status)

-  Numeric and other non-textual variables in VCL can now be assigned to
   textual variables; they will be converted as needed.

-  VCL scripts can now apply regular expression substitutions to textual
   variables using the regsub function.

-  A new management command, status, returns the state of the child.

-  Varnish will now build and run on Mac OS X.

varnishadm
----------

-  This is a new utility which sends a single command to a Varnish
   server's management port and prints the result to stdout, greatly
   simplifying the use of the management port from scripts.

varnishhist
-----------

-  The user interface has been greatly improved; the histogram will be
   automatically rescaled and redrawn when the window size changes, and
   it is updated regularly rather than at a rate dependent on the amount
   of log data gathered. In addition, the name of the Varnish instance
   being watched is displayed in the upper right corner.

varnishncsa
-----------

-  In addition to client traffic, varnishncsa can now also process log
   data from backend traffic.

-  A bug that would cause varnishncsa to segfault when it encountered an
   empty HTTP header in the log file has been fixed.

varnishreplay
-------------

-  This new utility will attempt to recreate the HTTP traffic which
   resulted in the raw Varnish log data which it is fed.

varnishstat
-----------

-  Don't print lifetime averages when it doesn't make any sense—for
   instance, there is no point in dividing the amount in bytes of free
   cache space by the lifetime in seconds of the varnishd process.

-  The user interface has been greatly improved; varnishstat will no
   longer print more than fits in the terminal, and will respond
   correctly to window resize events. The output produced in one-shot
   mode has been modified to include symbolic names for each entry. In
   addition, the name of the Varnish instance being watched is displayed
   in the upper right corner in curses mode.

varnishtop
----------

-  The user interface has been greatly improved; varnishtop will now
   respond correctly to window resize events, and one-shot mode (-1)
   actually works. In addition, the name of the Varnish instance being
   watched is displayed in the upper right corner in curses mode.

===========================
Changes from 1.0.3 to 1.0.4
===========================

varnishd
--------

-  The request workflow has been redesigned to simplify request
   processing and eliminate code duplication. All codepaths which need
   to speak HTTP now share a single implementation of the protocol. Some
   new VCL hooks have been added, though they aren't much use yet. The
   only real user-visible change should be that Varnish now handles
   persistent backend connections correctly (see `ticket
   #56 <http://varnish.projects.linpro.no/ticket/56>`_).

-  Support for multiple listen addresses has been added.

-  An "include" facility has been added to VCL, allowing VCL code to
   pull in code fragments from multiple files.

-  Multiple definitions of the same VCL function are now concatenated
   into one in the order in which they appear in the source. This
   simplifies the mechanism for falling back to the built-in default for
   cases which aren't handled in custom code, and facilitates
   modularization.

-  The code used to format management command arguments before passing
   them on to the child process would underestimate the amount of space
   needed to hold each argument once quotes and special characters were
   properly escaped, resulting in a buffer overflow. This has been
   corrected.

-  The VCL compiler has been overhauled. Several memory leaks have been
   plugged, and error detection and reporting has been improved
   throughout. Parts of the compiler have been refactored to simplify
   future extension of the language.

-  A bug in the VCL compiler which resulted in incorrect parsing of the
   decrement (-=) operator has been fixed.

-  A new -C command-line option has been added which causes varnishd to
   compile the VCL code (either from a file specified with -f or the
   built-in default), print the resulting C code and exit.

-  When processing a backend response using chunked encoding, if a chunk
   header crosses a read buffer boundary, read additional bytes from the
   backend connection until the chunk header is complete.

-  A new ping\_interval run-time parameter controls how often the
   management process checks that the worker process is alive.

-  A bug which would cause the worker process to dereference a NULL
   pointer and crash if the backend did not respond has been fixed.

-  In some cases, such as when they are used by AJAX applications to
   circumvent Internet Explorer's over-eager disk cache, it may be
   desirable to cache POST requests. However, the code path responsible
   for delivering objects from cache would only transmit the response
   body when replying to a GET request. This has been extended to also
   apply to POST.

   This should be revisited at a later date to allow VCL code to control
   whether the body is delivered.

-  Varnish now respects Cache-control: s-maxage, and prefers it to
   Cache-control: max-age if both are present.

   This should be revisited at a later date to allow VCL code to control
   which headers are used and how they are interpreted.

-  When loading a new VCL script, the management process will now load
   the compiled object to verify that it links correctly before
   instructing the worker process to load it.

-  A new -P command-line options has been added which causes varnishd to
   create a PID file.

-  The sendfile\_threshold run-time parameter's default value has been
   set to infinity after a variety of sendfile()-related bugs were
   discovered on several platforms.

varnishlog
----------

-  When grouping log entries by request, varnishlog attempts to collapse
   the log entry for a call to a VCL function with the log entry for the
   corresponding return from VCL. When two VCL calls were made in
   succession, varnishlog would incorrectly omit the newline between the
   two calls (see `ticket
   #95 <http://varnish.projects.linpro.no/ticket/95>`_).

-  New -D and -P command-line options have been added to daemonize and
   create a pidfile, respectively.

-  The flag that is raised upon reception of a SIGHUP has been marked
   volatile so it will not be optimized away by the compiler.

varnishncsa
-----------

-  The formatting callback has been largely rewritten for clarity,
   robustness and efficiency.

   If a request included a Host: header, construct and output an
   absolute URL. This makes varnishncsa output from servers which handle
   multiple virtual hosts far more useful.

-  The flag that is raised upon reception of a SIGHUP has been marked
   volatile so it will not be optimized away by the compiler.

Documentation
-------------

-  The documentation—especially the VCL documentation—has been greatly
   extended and improved.

Build system
------------

-  The name and location of the curses or ncurses library is now
   correctly detected by the configure script instead of being hardcoded
   into affected Makefiles. This allows Varnish to build correctly on a
   wider range of platforms.

-  Compatibility shims for clock\_gettime() are now correctly applied
   where needed, allowing Varnish to build on MacOS X.

-  The autogen.sh script will now correctly detect and warn about
   automake versions which are known not to work correctly.
