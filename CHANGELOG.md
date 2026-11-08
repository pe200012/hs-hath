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

### Changed
- **Filesystem backend**: hits are sent from the file instead of being read into memory, and the least recently served files are evicted to stay under the disk limit from the H@H settings page (100 MiB below it, as the official client). Its per-request in-memory LRU, which never survived past one request, is gone
- **Metrics**: `hath_latency_seconds` (a gauge holding the last request's latency in microseconds) is replaced by the histogram `hath_request_duration_seconds`; the always-zero `hath_errors_total` is removed
- `client-login.example` uses the Filesystem backend

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
