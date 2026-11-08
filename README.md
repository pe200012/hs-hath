# hs-hath - A Haskell Implementation of the Hentai@Home Client

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

A high-performance Haskell implementation of the Hentai@Home client. The client caches and serves gallery files, communicates with the H@H server via TLS-secured RPC, and supports S3-compatible storage backends including Cloudflare R2.

## Features

Files are cached on the local filesystem, in Cloudflare R2 (or another S3-compatible store), or in SQLite; see [Cache backends](#cache-backends). Cache misses are streamed to the requester while they are fetched. RPC replies are verified against the client key (protocol 1.6.6). Flood control matches the official client. Prometheus metrics are available at the `/metrics` endpoint. The client tracks build information via the `--version` flag.

## Installation

### Prerequisites
- Stack (Haskell toolchain)
- SQLite 3
- OpenSSL libraries

### Building
```bash
git clone https://github.com/pe200012/hs-hath.git
cd hs-hath
stack build
```

## Configuration

Create a `client-login` file with your credentials:
```dhall
{ clientId = "your-client-id"
, key = "your-api-key" 
, version = "1.0.0"
, proxy = None ClientProxy
, downloadDir = "./downloads"
, cachePath = "./cache.db"
}
```

See `client-login.example` for every field, including `cacheBackend`.

### Cache backends
- `CacheBackendFilesystem` (recommended for local disks): files live under `cachePath` as `xxxx/<file id>` and are sent straight from disk. The least recently served files are evicted to stay under the disk limit set on the H@H settings page.
- `CacheBackendR2`: hits are answered with presigned redirects to the bucket; nothing is evicted.
- `CacheBackendSQLite`: images are stored inside the database file at `cachePath`. Nothing is evicted, and every hit is a database write; kept for existing setups.

## Usage

Start the client:
```bash
stack exec hs-hath
```

View version information:
```bash
stack exec hs-hath -- --version
```

The client responds to `SIGINT` and `SIGTERM` for graceful shutdown. Settings reload automatically via RPC command. The Prometheus metrics endpoint is available at `/metrics` for monitoring.

## Architecture

The RPC client handles communication with the H@H server, including certificate management and settings synchronization. An HTTP server built on Warp serves cached resources from one of the cache backends above, with flood control in front of it.

## Metrics

The client exports Prometheus metrics in text format at the `/metrics` endpoint. Metrics include request counters, cache statistics, and operational counters for monitoring and alerting.

## Performance

Has not been optimized yet. Currently, it will take ~200MiB RAM on average.

## Troubleshooting

### Certificate errors
Verify system clock synchronization and certificate file permissions. Certificate refresh occurs automatically every 24 hours.

### Connection issues
Check proxy settings if configured. Ensure firewall allows outbound TLS connections on the required ports.

### Storage backend errors
For S3-compatible backends like Cloudflare R2, verify that the storage server returns required headers: `ETag`, `Content-Length`, and `Last-Modified`. Missing `ETag` headers cause write failures.

### Rate limiting
The client implements strict request throttling. Banned IPs and rate limit violations appear in console logs. Verify client configuration if legitimate requests are being rejected.

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.
