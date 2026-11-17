# Changelog for `hs-hath`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Upgrade notes
- **Reverse proxy**: behind a reverse proxy, run hs-hath with `--trust-proxy-headers`. Server commands are now accepted only from the addresses `rpc_server_ip` lists, taken from the peer as flood control sees it; without the option the proxy's address is not one of them and every `/servercmd/` request is refused (403)
- **`client_start` failures**: when `client_start` fails for a reason other than `FAIL_OTHER_CLIENT_CONNECTED` (another client connected) or `FAIL_CID_IN_USE` (client id in use), for example `FAIL_CONNECT_TEST` or no reply, the process stays up without announcing itself, as the official client does, instead of retrying every second. A supervisor will not restart it
- **Blacklist**: blacklisted files are now deleted from the cache (SQLite, Filesystem and R2); before, the list was only printed
- **Certificate**: a certificate with less than 24 h left is refused at start
- **Gallery layout**: galleries are downloaded into a new directory layout, `<downloadDir>/<title> [<gid>]` (see Changed); files under the old `<downloadDir>/<gid>/` are not recognised, so those galleries are fetched again
- **`/h/` requests** need valid `fileindex` and `xres` options even for cached files (404 otherwise), as with the official client
- **Library API**: modules and exports were removed, and signatures changed (see Changed). Removed modules: `Hath.Metrics.Counter`, `Hath.Metrics.Gauge`, `Hath.Metrics.Histogram`, `Hath.Metrics.Prometheus`, `Hath.Metrics.Registry`, `Hath.Metrics.Snapshot` and `Hath.Metrics.Types` (`Hath.Stats` holds the counters and renders them). Removed exports: from `Hath.Stats` the `Stats` effect (`Stats(..)`, `runStats`, `readPrometheus`) and the fields of `StatsEnv`; `Hath.Cache.SQLite.FileRecord`; `Hath.Locate.streamUpstream`; `Hath.Gallery.Downloader.downloadNextGallery`; the re-exports of `CacheRunner(..)` and `GalleryTask(..)` from `Hath.Server`; the constructor and fields of `Hath.Server.FloodControl.IPRecord` and its `Hashable IP` instance; the `galleryFileCount` field of `Hath.Gallery.GalleryMetadata`; the `optShowVersion` field of `Hath.CLI.Options` (the option parser answers `--version` itself); `Hath.Server.Middleware.normalizeAcceptMiddleware` and `withHathHeaders`

### Added
- **Connection limit**: as in the official client, a new connection is closed without a reply while more than `max_connections` others are open (the server's `max_connections`, or else 20 plus one per 10,000 bytes/s of `throttle_bytes`, up to 500). Local addresses and the RPC servers are never refused, but count. With more than 80 % taken, the RPC server is told (`act=overload`), at most every 30 s and without holding up the connection. Warp only knows the peer's address, so behind a reverse proxy on a local address nothing is limited
- `max_allowed_filesize` from the server's settings (1 GiB unless it says otherwise) is enforced: a cache miss, a gallery file or a `threaded_proxy_test` download larger than it is not fetched
- **`galleryinfo.txt`**: once all of a gallery's files are there, the list's INFORMATION section is written to it
- Library modules `Hath.ServerTime` (`serverNow`, `setServerTime`), `Hath.Download` (`fetchFile`, and `receive`, moved from `Hath.Locate` and given the time a read may wait) and `Hath.Server.ConnectionLimit`

### Changed
- **Gallery layout** as in the official client: a gallery goes into `<downloadDir>/<title> [<gid>]`, or `[<gid>-<xres>x]` for a resized one, its title cleaned up the same way and cut short with `...` to `max_filename_length` (from the server's settings, 125 by default); it used to be `<downloadDir>/<gid>/`. If the file system will not take the name, the directory is `<gid>` or `<gid>-<xres>x`. A file already there and intact is kept, a damaged one deleted
- **Certificate refresh** swaps the certificate in place for new handshakes instead of restarting the listener, so transfers in progress are no longer cut off and `client_start` is not re-sent
- **Filesystem backend**: hits are sent from the file instead of being read into memory, and the least recently served files are evicted to stay under the disk limit from the H@H settings page (100 MiB below it, as the official client). Its per-request in-memory LRU, which never survived past one request, is gone
- **Metrics**: `hath_latency_seconds` (a gauge holding the last request's latency in microseconds) is replaced by the histogram `hath_request_duration_seconds`; the always-zero `hath_errors_total` is removed. `/metrics` is rendered when it is read, not every 5 seconds by a background thread, so it shows current values and `hath_uptime_seconds`, which was always 0, counts; the other names, their order and the format are unchanged
- `client-login.example` uses the Filesystem backend
- **Logs**: failed TLS handshakes and connections that close before sending anything (scanners probing SSL3/TLS 1.0/1.1 and cipher suites, a few thousand a day on a public node) are no longer printed. The TLS 1.0/1.1 entries are dropped from the allowed versions; `tls` 2.x rejected them anyway
- **TLS session resumption** (session IDs on TLS 1.2, tickets on TLS 1.3): a returning visitor skips the certificate and its RSA signature, which cuts the server's CPU per handshake by about 45 % on TLS 1.3 and 75 % on TLS 1.2 (several ms on a small VM)
- **Internals**: the modules live under `Hath.*`, split by subject
- `/admin/settings` lists only the settings the server assigns
- The SQLite backend no longer records the file name a file was first requested under, and a hit no longer writes to the database (the `lru_counter` column, which nothing read, stays as it is)
- **R2 backend**: a file just uploaded after a cache miss is redirected at once on its next request, instead of first asking the bucket whether it is there (a 40–350 ms round trip)
- `hath-bench` writes the hs-hath config through `ClientConfig`'s `ToDhall` instance instead of a hand-written copy of the record
- **RPC requests** are sent as the official client sends them: the query carries `clientbuild`, `act`, `add`, `cid`, `acttime`, `actkey` in that order, `add=` even when empty, with the `;` inside `add` unescaped. Requests to the RPC server, to image servers (cache misses) and for gallery files carry `User-Agent: Hentai@Home 1.6.6` and, as in the official client, `Connection: Close` (RPC, gallery files) or `Hath-Request` (image servers), and nothing else: no longer our response headers (`Cache-Control`, `Server`, `X-Content-Type-Options`) or `Accept-Encoding: gzip`
- **RPC transport**: a request whose exchange fails (no connection, a broken reply, an HTTP error other than 404/410) is sent again at once, three times in all, and a connection attempt gives up after 5 s, as in the official client. The reply may still take up to 60 s
- **Responses** carry the official client's headers: `Server`, `Connection: close`, `Content-Type` and `Content-Length`, and `Cache-Control` only with a body; no more `User-Agent` or `X-Content-Type-Options`. An empty body says `Content-Length: 0` where the official client says nothing and closes. An error is the official page, `An error has occurred. (<status>)` as `text/html; charset=ISO-8859-1`; so are the replies of server commands, and `robots.txt` is `text/plain; charset=ISO-8859-1`. A request other than GET or HEAD gets 400. As in the official client, the path is not percent-decoded except for `%3d`. Flood control's 429, and Range and If-Modified-Since for files from disk, are as they were
- **Library API** (RPC): `CallRPC` and `CallDownloader` take `add` as `Text`, empty when there is none; `HathError`'s `TransportError` carries a `Text`; `serverStat` returns `()` and fails unless the server answers OK; `clientLogin` and `clientSettings` need `Embed IO`; `Hath.Protocol.clientBuild` is an `Int64`. New: `Hath.Settings.settingNumber`, `Hath.Protocol`'s `rpcQuery`, `outgoingHeaders` and `retryWithServerTime`, `Hath.RPC.worthRetrying`
- **Library API** (stats, R2): `Hath.Stats` counts with plain functions over `Reader StatsEnv` (no effect to interpret) and adds `readMetrics`, `renderMetrics`, `trackConnection` and `observeDuration`; `App` no longer carries `Reader ServerEnv` or `AtomicState HathSettings`; `Hath.Cache.R2.putR2Object` uploads a file (the cache and `migrate-to-r2` share it)
- **Library API** (lifecycle): `Hath.Cache.Cache` has `DeleteFile` (`deleteFile`); `Hath.RPC.clientStart` returns `()`, and it, `clientStop` and `stillAlive` fail with the server's status unless it answers OK; `getBlacklist` returns `FileURI`s; new `Hath.RPC.StartFailure`, `startFailure` and `toldToStop`, and `Hath.Server.needsRenewal`; `startServer` returns after a shutdown instead of exiting. The library depends on `hourglass` (crypton-x509's date type)
- **Library API** (HTTP): `Hath.Server.API` reads paths (`Route`, `route`, `ServerCommand`, `ProxyTest`, `parseServerCommand`) instead of declaring a servant API; `Hath.Server.Handlers` exports the WAI `application` instead of `server`, which took the `ServerEnv`; `App` ends a request by throwing a `Status` instead of a `ServerError`, and `runApp` returns `IO (Either Status a)`. `Hath.Protocol`: `responseHeaders`, `keystampValid`, `commandTimeValid` and `fetchParams` replace `hathHeaders`, `parseKeystamp`, `maxKeystampDrift` and `maxCommandDrift`; `hathRequestToken` takes the file id as requested, as does `Hath.RPC.fileSources` (now `fileSources fileIndex xres fileId`) and `Hath.Locate.locateResource`. `Hath.FileURI.parseFileURI` takes only the ids the official client takes, and `renderFileURIBytes` writes an id without a resolution in three parts. New: `Hath.Server.FloodControl.isRPCServer`
- **Library API** (cache misses, connection limit): `Hath.Locate.locateResource` takes `max_allowed_filesize` and returns `Either Status Located` (the status for the reader) instead of `Maybe Located`; new are `Hath.Locate.usableReply`, `Hath.RPC.httpManager` (one manager for RPC, proxy tests, gallery files and cache misses, whose connections give up after 5 s), `connectMicros` and `overload`, `Hath.Settings.maxConnections`, and the `HathSettings` fields `maxConnectionsOverride` and `maxAllowedFileSize`
- **Library API** (gallery downloader): `runGalleryDownloader` takes the settings and an `MVar ()` that `start_downloader` fills (`ServerEnv`'s `envDownloadRequest`, replacing `envGalleryTasks`; `GalleryTask` is gone); `downloadQueue` (one run) and `anotherRound` are exported, `galleryFilePath` is not. `GalleryMetadata` has `galleryTitle` (cleaned up) and `galleryInformation`; new `Hath.Gallery.galleryDirectoryName` and `fallbackDirectoryName`, and `HathSettings`' `maxFilenameLength`. `Hath.RPC.nextGallery` takes the gallery to mark done, `galleryFileSource` replaces `galleryFileSources`, `failureReport` is new and `completeGallery` is gone
- **TLS 1.3 early data (0-RTT)** is no longer accepted: a request sent in early data can be replayed. The vendored warp-tls is 3.4.18 (was 3.4.13, which took up to 2018 bytes of it), still with the one added line that lets TLS 1.2 clients without the Extended Main Secret extension connect. It needs `network` 3.2.12 or later, so `stack.yaml` and `stack-release.yaml` take `network-3.2.14.0` as an extra dependency

### Fixed
- **Settings refresh** (`refresh_settings`) reset every setting the reply did not repeat to its default; it now updates only what it carries
- **Filesystem backend** could not start (the startup cache check opened `cachePath` as an SQLite file); the check now only runs for the SQLite backend
- Files are written through a temp file and renamed, so a crash no longer leaves a truncated file that later gets served (gallery downloads and the Filesystem cache). Truncated files left by older versions are dropped and refetched
- **`downloadDir`** and `--download-dir` had no effect: the gallery downloader always wrote to `./download`. It now writes to the configured directory
- The CI workflow now runs the test suite
- The release build (`stack-release.yaml`) pointed at directories that no longer exist
- **Clock skew**: keystamps, server commands, speed tests and the signatures of our RPC requests are now checked against the RPC server's clock, taken from the `server_time` of the `server_stat`, `client_login` and `client_settings` replies, as the official client does. A node whose clock is off no longer fails to log in or refuses readers with "time is out of sync". A request without arguments that gets `KEY_EXPIRED` takes the time from `server_stat` and is sent once more
- **`server_stat`**: a reply other than a signed OK (`FAIL`, a forged reply) no longer counts as the server being available. A server that requires a newer client (`min_client_build` above 181) stops hs-hath at startup with a message saying so; a newer client being out (`cur_client_build`) is logged as a warning
- **Start order**: the port is bound before `client_start` is sent, so the server's connectivity test no longer races the listener. `client_start` is sent once, not every second until it succeeds; `FAIL_OTHER_CLIENT_CONNECTED` and `FAIL_CID_IN_USE` stop hs-hath with the official client's explanation (any other failure: see Upgrade notes). `client_settings` is fetched once after it, and `still_alive`, the blacklist and the certificate check only start once it went through
- **Shutdown** no longer cuts transfers off: as in the official client, `client_stop` is sent once, and only if `client_start` went through; then hs-hath serves 5 s more, stops accepting connections and waits up to 25 s for open ones. It used to stop serving first, then send `client_stop` up to three times
- **`still_alive`** is sent every 110 s instead of every minute, as in the official client; `TERM_BAD_NETWORK` shuts the client down as above, and other failures are logged instead of dropped
- **Blacklist** as in the official client: fetched once after `client_start` (the last three days), then every 6 hours (the last 12 hours), instead of before `client_start` and every 36 minutes; only the number of files is logged
- **Certificate expiry**: hs-hath looks at the certificate every 5 minutes; with less than a day left it fetches a new one and swaps it in, and shuts down as above if none comes or the new one expires within a day too (also after `refresh_certs`)
- **Keystamps** are accepted while less than 15 minutes from the server's time either way, as in the official client (it was 10 minutes). Speed tests (`/t/`) allow 5 minutes, as server commands do (it was 10), and work without the part after the key
- **HEAD** goes through the same checks as GET and gets the same status and headers (for a file its own Content-Type and Content-Length), without the body. A HEAD for a file not in the cache fetches it into the cache, as in the official client. Any HEAD used to get 200 unchecked
- **Server commands** are only taken from the RPC servers' addresses (see Upgrade notes), as in the official client. A command with a part missing gets 403; the command name is read in any case; an unknown command, or one whose parameters cannot be read, is answered `INVALID_COMMAND` with 200 (it was 404 or 403); `refresh_settings` answers 200 and nothing whether or not the refresh worked (a failed one was 500)
- **File ids** are checked as the official client checks them, and a malformed one gets 404 without the RPC server being asked: a lower-case SHA-1, the five-part form or the three-part one without a resolution (which used to get 404), a size that fits a Java int, one of the eight types. `fileindex` must be a number and `xres` a number or `org`, also for a file in the cache. `srfetch` and `Hath-Request` carry the file id as the reader wrote it, not as we would write it. `/h/<file id>/<options>` is served without the file name after it, as by the official client
- **`threaded_proxy_test`** downloads go through the same code as gallery files (`Hath.Download.fetchFile`), as in the official client: a connection gets 5 s and every read 10 s, a download three attempts (none after a 404 or 410), `User-Agent` and `Connection: Close` are sent, and a reply larger than `max_allowed_filesize` fails. The time is taken from the download's first byte to its end (it counted from before connecting), and a download counts if its body had the announced length and at least the test size. All of a test's attempts stay within 60 s
- **Cache misses** are downloaded on a thread of their own, as by the official client: a reader who goes away no longer aborts the download, and the file is still checked (length, SHA-1) and cached. It is held in memory once, in one buffer that the reader is relayed from and the cache receives; it used to be kept as chunks and then copied once more
- **Upstream timeouts on a cache miss**: a source gets 5 s to connect and 30 s for its headers, every read 30 s, and the whole body 300 s, as in the official client; a stalled image server used to hold the reader and the upstream connection indefinitely. When the download breaks off (upstream closes early, stalls or runs out of time), the reader's connection is dropped at once
- **Cache miss statuses** as in the official client: 404 when `srfetch` fails or names no source (a failed `srfetch` gave 500), 502 if a source answered with something other than the file, 500 if none could be reached (both gave 404)
- **Gallery downloads longer than five minutes**: every request of one download carried the time the download began, so the server came to refuse them as `KEY_EXPIRED`. Each request now carries its own time
- Errors that end a gallery download are logged; they used to be dropped
- An option value containing `=` (in `/h/` and `/servercmd/` URLs) no longer fails the request
- **Gallery downloader queue**: one `start_downloader` works through the whole queue, as in the official client. Each `fetchqueue` after the first marks the gallery before it done, whether or not all its files came, and brings the next one, until the server answers `NO_PENDING_DOWNLOADS`. It used to download one gallery per `start_downloader`, throw away the next gallery the server had sent along, and never mark a gallery with a failed file done, so the server sent that one again and again. A failed `dlfetch` is one failed attempt at that file; it used to end the gallery. A `start_downloader` that comes during a run is ignored, as by the official client (the run picks up what was queued meanwhile)
- **Gallery retries and failure reports** as in the official client: the files still missing are gone through again, for at most nine rounds and while fewer downloads have failed than twice the gallery's number of files, with a pause of 1 s after each download and 5 s after each failure. `dlfetch` numbers the attempts at a file from 1 (it sent 0 to 3), and only the first source it names is tried. Every failure, a broken download as well as a wrong SHA-1, is recorded as `host-fileindex-xres`; the distinct ones are reported in one `dlfails` per gallery, and only if there are 1 to 50 of them. An empty `dlfails` used to follow every failed file
- **Gallery files** are checked as the official FileDownloader checks them: a status below 400, a Content-Length that is present, at most `max_allowed_filesize` and matched by the body, up to three attempts (none after a 404 or 410), 5 s to connect and 10 s for every read. An error page or a cut-off body used to be saved as the image when the list gives no SHA-1 for it
- `hath_download_task_count_total` counts the galleries taken from the queue; it counted every look at the queue, empty ones included

### Removed
- The library API removals are listed under Upgrade notes
- The disabled static-range check: like the official client, any file the server routes here is served
- The dependencies `servant`, `servant-server`, `servant-client`, `servant-client-core` and `http-media`: requests are routed by their path, as the official client routes them, and the RPC client uses `http-client` directly
- The dependencies `extra`, `http-conduit` and `unordered-containers` of hs-hath itself (the vendored `minio-hs` keeps its own), `mason`, `timers`, `suspend`, a dozen more that nothing imported, and the pinned `random`; `crypton-connection` is only a dependency of `hath-bench` (and of the vendored `minio-hs`)
- The vendored `polysemy-zoo`, the internal library `hs-hath-placeholder` (vendored `placeholder`), the parts of the vendored `minio-hs` that are not built, and `Setup.hs`

## [1.2.0] - 2026-11-08

### Security
- **Gallery downloader**: file names from the server could contain path separators, letting a forged list write anywhere the client can. Lists must now carry a valid `DLHASH`, and names/types are limited to what the gallery system produces (official 1.6.6 fix)

### Changed
- **Protocol 1.6.6 (build 181)**: RPC moves to `/16/rpc` and `/16/dl`; replies must be signed with the client key and unsigned or altered ones are rejected. The build is fixed in code; the config `version` field is no longer used
- **Flood control** now matches the official client (10 hits draining 1/s, 60 s block; local network and RPC servers exempt)
- **Cache misses** are streamed to the requester while being fetched instead of after the full download
- **Hackage**: builds without extra flags; `cryptostore` is vendored with crypton baked in

### Fixed
- `threaded_proxy_test` reports real results; `servercmd` signatures are checked
- Files whose hash is `unknown` (generated on demand) can now be downloaded
- webp/avif Content-Type; a failed cache write no longer fails the request; shutdown no longer spins when the RPC server is unreachable

### Added
- `hath-bench`: a mock RPC server and benchmark that drives hs-hath or the official client

## [1.1.2] - 2026-01-31

### Changed
- Now vendors `minio-hs`, `placeholder`, `polysemy-zoo`, and `warp-tls` as internal libraries for finer version control and local modifications
- Update tested GHC version
- Increased presigned URL expiry from 10 to 20 minutes and adjusted local cache TTL to 10 minutes
- Removed custom keystamp-based rate limiting
- Updated to lts-24.27

## [1.1.1] - 2026-01-26
### Added
- R2 Object Storage support with client-side caching
- Presigned URL generation for R2 redirects
### Changed
- Refactored time handling to use MonotonicTime for better precision
- Fix rate limiting logic
- Drop macOS support (removed logic-macos.hs)

## [1.0.3] - 2026-01-24

### Added
- Custom Prometheus metrics implementation with counters and gauges
- Metrics registry and snapshot mechanism for efficient metrics collection
- Non-blocking metrics retrieval endpoint with background worker
- stack-release.yaml for optimized release builds with LLVM support
- Aggressive optimization options for release builds

### Changed
- **Performance:** Improved rate limiting by removing TVar dependency and simplifying architecture
- **Performance:** Refactored to use monotonic time, avoiding complex UTCTime calculations
- Removed development logging statements
- Removed redundant imports
- Automated release process with GitHub Actions (separate build and release jobs)
- Removed dependency on `prometheus` and `binary` packages in favor of custom implementation

### Fixed
- Fixed config validation option to take effect properly

## [1.0.2] - 2026-01-23

### Added
- Gallery downloader task queue for better task management
- Filesystem cache backend as an alternative storage option
- Admin settings endpoint for runtime configuration
- Configuration validation and LRU cache tuning capabilities
- Enhanced logging for gallery downloader errors and failed tasks

### Changed
- **Performance:** Refactored hashing to use type classes for better abstraction
- **Performance:** Reduced intermediate ByteString allocations in hot paths
- **Performance:** Shared global HTTP manager across requests to reduce overhead
- **Performance:** Optimized buffer allocation strategies
- **Performance:** Improved hashing performance for security tokens
- Switched to monotonic clock for more accurate uptime tracking
- Refactored server loop for better code structure and maintainability
- Improved download retry logic with better error handling
- Improved gallery download resilience and error recovery
- Split middlewares into separate modules for better organization
- Enhanced CI workflow for Haskell projects

### Fixed
- Fixed keystamp rate limiting implementation
- Bypassed staticRanges check in Locate.hs to resolve compatibility issues
- Worked around encoding errors in gallery processing
- Added existence checks before processing to avoid redundant work

## [1.0.1] - 2026-01-20

### Added
- Statistics tracking with Prometheus metrics integration
- Prometheus package dependency for metrics collection
- Gallery downloader implementation
- Stats effect constructors for new metrics types
- Release workflow automation (release.yml)
- GHC build caching in CI

### Changed
- Refactored TrafficStats from TVar to Prometheus Registry
- Refactored settings handling for dynamic runtime updates
- Refactored signal handling and removed verification step
- DRY improvements for LRU cache implementation
- Refactored module structures for better organization
- Updated README with comprehensive documentation

### Fixed
- Fixed threaded speedtest issues

### Removed
- Removed Windows release targets

## [1.0.0] - 2026-01-19

### Added
- Initial release with keystamp-based rate limiting
- Core H@H (Hentai@Home) server functionality
- RPC client for H@H network communication
- SQLite and R2 storage backends with runtime switching
- File integrity verification with streaming to prevent space leaks
- In-memory LRU cache for R2 backend
- Docker support for containerized deployment
- Trust-proxy-headers option for real IP handling
- Migrate-to-R2 tool for backend migration
- Command-line interface with configuration options
- Traffic statistics tracking and reporting
- Graceful shutdown handling
- TLS support with certificate management
- Environment variable configuration (HATH_RPC_HOST, base URL)

### Changed
- Upgraded to GHC 9.6.7 and resolver lts-22.44
- SQLite journal mode changed to delete for better compatibility
- Switched from static linking to dynamic linking
- Widened aeson version bounds for better compatibility

### Fixed
- Resolved space leak in checkRate function
- Fixed space leak in file verification by using streaming approach
