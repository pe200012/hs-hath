# Changelog for `hs-hath`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Fixed
- **Settings refresh** (`refresh_settings`) reset every setting the reply did not repeat to its default; it now updates only what it carries
- **Filesystem backend** could not start (the startup cache check opened `cachePath` as an SQLite file); the check now only runs for the SQLite backend
- Files are written through a temp file and renamed, so a crash no longer leaves a truncated file that later gets served (gallery downloads and the Filesystem cache). Truncated files left by older versions are dropped and refetched
- Removed the disabled static-range check: like the official client, any file the server routes here is served
- The CI workflow now runs the test suite
- **`downloadDir`** and `--download-dir` had no effect: the gallery downloader always wrote to `./download`. It now writes to the configured directory, as `<downloadDir>/<gallery id>/`; files of a download in progress under `./download` are fetched again unless that is the configured directory
- `hath_uptime_seconds` was always 0; it is now computed when `/metrics` is read
- **Gallery downloads longer than five minutes**: every request of one download carried the time the download began, so the server came to refuse them as `KEY_EXPIRED`. Each request now carries its own time
- Errors that end a gallery download are logged; they used to be dropped
- An option value containing `=` (in `/h/` and `/servercmd/` URLs) no longer fails the request
- The release build (`stack-release.yaml`) pointed at directories that no longer exist
- **Clock skew**: keystamps, server commands, speed tests and the signatures of our RPC requests are now checked against the RPC server's clock, taken from the `server_time` of the `server_stat`, `client_login` and `client_settings` replies, as the official client does. A node whose clock is off no longer fails to log in or refuses readers with "time is out of sync". A request without arguments that gets `KEY_EXPIRED` takes the time from `server_stat` and is sent once more
- **`server_stat`**: a reply other than a signed OK (`FAIL`, a forged reply) no longer counts as the server being available. A server that requires a newer client (`min_client_build` above 181) stops hs-hath at startup with a message saying so; a newer client being out (`cur_client_build`) is logged as a warning
- **Start order**: the port is bound before `client_start` is sent, so the server's connectivity test no longer races the listener. `client_start` is sent once instead of every second until it succeeds: `FAIL_OTHER_CLIENT_CONNECTED` and `FAIL_CID_IN_USE` stop hs-hath with the official client's explanation; after any other failure (`FAIL_CONNECT_TEST`, no reply) it stays up unannounced so that the port can be tested, as the official client does. `client_settings` is fetched once after it, and `still_alive`, the blacklist and the certificate check only start once it went through
- **Shutdown** no longer cuts transfers off: as in the official client, `client_stop` is sent once, and only if `client_start` went through; then hs-hath serves 5 s more, stops accepting connections and waits up to 25 s for open ones. It used to stop serving first, then send `client_stop` up to three times
- **`still_alive`** is sent every 110 s instead of every minute, as in the official client; `TERM_BAD_NETWORK` shuts the client down as above, and other failures are logged instead of dropped
- **Blacklist**: blacklisted files are deleted from the cache (all three backends) instead of the list being printed; only their number is logged. It is fetched once after `client_start` (the last three days) and then every 6 hours (the last 12 hours), instead of before `client_start` and every 36 minutes
- **Certificate expiry**: hs-hath refuses to start with a certificate that expires within a day, and looks again every 5 minutes; with less than a day left it fetches a new one and swaps it in, and shuts down as above if none comes or the new one expires within a day too (also after `refresh_certs`)
- **Keystamps** are accepted while less than 15 minutes from the server's time either way, as in the official client (it was 10 minutes). Speed tests (`/t/`) allow 5 minutes, as server commands do (it was 10), and work without the part after the key
- **HEAD** goes through the same checks as GET and gets the same status and headers (for a file its own Content-Type and Content-Length), without the body. A HEAD for a file not in the cache fetches it into the cache, as in the official client. Any HEAD used to get 200 unchecked
- **Server commands** are only taken from an address that `rpc_server_ip` lists, the peer as flood control sees it (behind a reverse proxy that needs `--trust-proxy-headers`), as in the official client. A command with a part missing gets 403; the command name is read in any case; an unknown command, or one whose parameters cannot be read, is answered `INVALID_COMMAND` with 200 (it was 404 or 403); `refresh_settings` answers 200 and nothing whether or not the refresh worked (a failed one was 500)
- **File ids** are checked as the official client checks them: a lower-case SHA-1, the five-part form or the three-part one without a resolution (which used to get 404), a size that fits a Java int, one of the eight types. `fileindex` must be a number and `xres` a number or `org`, also for a file in the cache, as in the official client; neither is sent to the RPC server otherwise. `srfetch` and `Hath-Request` carry the file id as the reader wrote it, not as we would write it
- **A failed `srfetch`** leaves the reader with 404 instead of 500, as in the official client
- **`threaded_proxy_test`** times a download from its first byte to its end, as the official client does; it counted from before connecting. A connection gets 5 s and every read 10 s, a download three attempts (none after a 404), and it counts if its body had the announced length and at least the test size. All of a test's attempts stay within 60 s
- `/h/<file id>/<options>` is served without the file name after it, as by the official client
- **Cache misses** are downloaded on a thread of their own, as by the official client: a reader who goes away no longer aborts the download, and the file is still checked (length, SHA-1) and cached. It is held in memory once, in one buffer that the reader is relayed from and the cache receives; it used to be kept as chunks and then copied once more
- **Upstream timeouts on a cache miss**: a source gets 5 s to connect and 30 s for its headers, every read 30 s, and the whole body 300 s, as in the official client; a stalled image server used to hold the reader and the upstream connection indefinitely. When the download breaks off (upstream closes early, stalls or runs out of time), the reader's connection is dropped at once
- **A cache miss no source sends** gets 502 if a source answered (another status or length, an error after connecting) and 500 if none could be reached, as from the official client; it was 404 either way (it still is when `srfetch` gives no source). `max_allowed_filesize` from the server's settings (1 GiB unless it says otherwise) is enforced: a larger file is not fetched
- **Connection limit**: as in the official client, a new connection is closed without a reply while more than `max_connections` others are open (the server's `max_connections`, or else 20 plus one per 10,000 bytes/s of `throttle_bytes`, up to 500). Local addresses and the RPC servers are never refused, but count. With more than 80 % taken, the RPC server is told (`act=overload`), at most every 30 s and without holding up the connection. Warp only knows the peer's address, so behind a reverse proxy on a local address nothing is limited

### Changed
- **Certificate refresh** swaps the certificate in place for new handshakes instead of restarting the listener, so transfers in progress are no longer cut off and `client_start` is not re-sent
- **Filesystem backend**: hits are sent from the file instead of being read into memory, and the least recently served files are evicted to stay under the disk limit from the H@H settings page (100 MiB below it, as the official client). Its per-request in-memory LRU, which never survived past one request, is gone
- **Metrics**: `hath_latency_seconds` (a gauge holding the last request's latency in microseconds) is replaced by the histogram `hath_request_duration_seconds`; the always-zero `hath_errors_total` is removed. `/metrics` is rendered when it is read, not every 5 seconds by a background thread, so it shows current values; names, order and format are unchanged
- `client-login.example` uses the Filesystem backend
- **Logs**: failed TLS handshakes and connections that close before sending anything (scanners probing SSL3/TLS 1.0/1.1 and cipher suites, a few thousand a day on a public node) are no longer printed. The TLS 1.0/1.1 entries are dropped from the allowed versions; `tls` 2.x rejected them anyway
- **TLS session resumption** (session IDs on TLS 1.2, tickets on TLS 1.3): a returning visitor skips the certificate and its RSA signature, which cuts the server's CPU per handshake by about 45 % on TLS 1.3 and 75 % on TLS 1.2 (several ms on a small VM)
- **Internals**: the modules live under `Hath.*`, split by subject (see the README). `mason`, `timers`, `suspend`, the vendored `polysemy-zoo` and a dozen dependencies nothing imported are gone, as is the pinned `random`
- A malformed file id is answered with 404 without asking the RPC server about it
- `/admin/settings` lists only the settings the server assigns
- The SQLite backend no longer records the file name a file was first requested under, and a hit no longer writes to the database (the `lru_counter` column, which nothing read, stays as it is)
- **R2 backend**: a file just uploaded after a cache miss is redirected at once on its next request, instead of first asking the bucket whether it is there (a 40–350 ms round trip)
- `hath-bench` writes the hs-hath config through `ClientConfig`'s `ToDhall` instance instead of a hand-written copy of the record
- **RPC requests** are sent as the official client sends them: the query carries `clientbuild`, `act`, `add`, `cid`, `acttime`, `actkey` in that order, `add=` even when empty, with the `;` inside `add` unescaped. Requests to the RPC server, to image servers (cache misses) and for gallery files carry `User-Agent: Hentai@Home 1.6.6` and, as in the official client, `Connection: Close` (RPC, gallery files) or `Hath-Request` (image servers), and nothing else: no longer our response headers (`Cache-Control`, `Server`, `X-Content-Type-Options`) or `Accept-Encoding: gzip`
- **RPC transport**: a request whose exchange fails (no connection, a broken reply, an HTTP error other than 404/410) is sent again at once, three times in all, and a connection attempt gives up after 5 s, as in the official client. The reply may still take up to 60 s
- **Library API** (RPC): `CallRPC` and `CallDownloader` take `add` as `Text`, empty when there is none; `HathError`'s `TransportError` carries a `Text`; `serverStat` returns `()` and fails unless the server answers OK; `clientLogin` and `clientSettings` need `Embed IO`; `Hath.Protocol.clientBuild` is an `Int64`. New: `Hath.ServerTime` (`serverNow`, `setServerTime`), `Hath.Settings.settingNumber`, `Hath.Protocol`'s `rpcQuery`, `outgoingHeaders` and `retryWithServerTime`, `Hath.RPC.worthRetrying`
- **Library API**: `Hath.Stats` counts with plain functions over `Reader StatsEnv` (no effect to interpret) and adds `readMetrics`, `renderMetrics`, `trackConnection` and `observeDuration`; `Hath.Server.Handlers.server` takes the `ServerEnv`, and `App` no longer carries `Reader ServerEnv` or `AtomicState HathSettings`; `Hath.Cache.R2.putR2Object` uploads a file (the cache and `migrate-to-r2` share it)
- **Library API** (lifecycle): `Hath.Cache.Cache` has `DeleteFile` (`deleteFile`); `Hath.RPC.clientStart` returns `()`, and it, `clientStop` and `stillAlive` fail with the server's status unless it answers OK; `getBlacklist` returns `FileURI`s; new `Hath.RPC.StartFailure`, `startFailure` and `toldToStop`, and `Hath.Server.needsRenewal`; `startServer` returns after a shutdown instead of exiting. The library depends on `hourglass` (crypton-x509's date type)
- **Responses** carry the official client's headers: `Server`, `Connection: close`, `Content-Type` and `Content-Length`, and `Cache-Control` only with a body; no more `User-Agent` or `X-Content-Type-Options`. An empty body says `Content-Length: 0` where the official client says nothing and closes. An error is the official page, `An error has occurred. (<status>)` as `text/html; charset=ISO-8859-1`; so are the replies of server commands, and `robots.txt` is `text/plain; charset=ISO-8859-1`. A request other than GET or HEAD gets 400. As in the official client, the path is not percent-decoded except for `%3d`. Flood control's 429, Range and If-Modified-Since for files from disk, `/metrics` and `/admin/settings` are as they were
- **Library API** (HTTP): `Hath.Server.API` reads paths (`Route`, `route`, `ServerCommand`, `ProxyTest`, `parseServerCommand`) instead of declaring a servant API; `Hath.Server.Handlers` exports the WAI `application` instead of `server`; `App` ends a request by throwing a `Status` instead of a `ServerError`, and `runApp` returns `IO (Either Status a)`. `Hath.Protocol`: `responseHeaders`, `keystampValid`, `commandTimeValid` and `fetchParams` replace `hathHeaders`, `parseKeystamp`, `maxKeystampDrift` and `maxCommandDrift`; `hathRequestToken` takes the file id as requested, as does `Hath.RPC.fileSources` (now `fileSources fileIndex xres fileId`) and `Hath.Locate.locateResource`. `Hath.FileURI.parseFileURI` takes only the ids the official client takes, and `renderFileURIBytes` writes an id without a resolution in three parts. New: `Hath.Server.FloodControl.isRPCServer`
- **Library API** (cache misses, connection limit): `Hath.Locate.locateResource` takes `max_allowed_filesize` and returns `Either Status Located` (the status for the reader) instead of `Maybe Located`; new are `Hath.Locate.usableReply` and `receive`, `Hath.RPC.httpManager` (one manager for RPC, proxy tests and cache misses, whose connections give up after 5 s), `connectMicros` and `overload`, `Hath.Settings.maxConnections` and the `HathSettings` fields `maxConnectionsOverride` and `maxAllowedFileSize`, and the module `Hath.Server.ConnectionLimit`

### Removed
- Modules `Hath.Metrics.Counter`, `Hath.Metrics.Gauge`, `Hath.Metrics.Histogram`, `Hath.Metrics.Prometheus`, `Hath.Metrics.Registry`, `Hath.Metrics.Snapshot` and `Hath.Metrics.Types`: `Hath.Stats` holds the counters and renders them
- From `Hath.Stats`: the `Stats` effect (`Stats(..)`, `runStats`, `readPrometheus`) and the fields of `StatsEnv`
- `Hath.Cache.SQLite.FileRecord`; `Hath.Locate.streamUpstream` and `Hath.Gallery.Downloader.downloadNextGallery` are no longer exported
- `Hath.Server` no longer re-exports `CacheRunner(..)` (from `Hath.Cache`) and `GalleryTask(..)` (from `Hath.Gallery.Downloader`)
- `Hath.Server.FloodControl`: the constructor and fields of `IPRecord`, and the `Hashable IP` instance
- The `galleryFileCount` and `galleryTitle` fields of `Hath.Gallery.GalleryMetadata`; the `optShowVersion` field of `Hath.CLI.Options` (the option parser answers `--version` itself)
- The internal library `hs-hath-placeholder` (vendored `placeholder`), the parts of the vendored `minio-hs` that are not built, `Setup.hs`, and the dependencies `extra`, `http-conduit` and `unordered-containers`; `crypton-connection` is only a dependency of `hath-bench`
- The dependencies `servant-client` and `servant-client-core`: the RPC client uses `http-client` directly
- The dependencies `servant`, `servant-server` and `http-media`: requests are routed by their path, as the official client routes them. `Hath.Server.Middleware.normalizeAcceptMiddleware` and `withHathHeaders`

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
