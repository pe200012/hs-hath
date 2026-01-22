# Changelog for `hs-hath`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

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
