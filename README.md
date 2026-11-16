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

Copy `client-login.example` to `client-login` and fill in your client id and key. The file is a [Dhall](https://dhall-lang.org) expression; the example defines the record with its defaults, so a configuration only names what differs:

```dhall
in  ClientConfig::{
    , clientId = "<YOURID>"
    , key = "<YOURKEY>"
    , version = "176"
    , downloadDir = "./download"
    , cachePath = "./cache"
    , cacheBackend = CacheBackend.CacheBackendFilesystem
    , r2Config = None R2Config
    }
```

| Field | Meaning |
| --- | --- |
| `clientId`, `key` | The client id and key from the H@H site |
| `downloadDir` | Where the gallery downloader puts galleries, as `<downloadDir>/<gallery id>/` |
| `cachePath` | The cache directory (Filesystem) or database file (SQLite); not used by R2 |
| `cacheBackend` | One of the [cache backends](#cache-backends) |
| `r2Config` | Endpoint and bucket for the R2 backend. The credentials come from the environment: `R2_ACCESS_KEY` and `R2_SECRET_KEY` |
| `version`, `proxy`, `lruCacheSize` | Not used. They are accepted so that existing files keep loading |

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

Options:

| Option | Effect |
| --- | --- |
| `-c`, `--config FILE` | The configuration file (default `./client-login`) |
| `-d`, `--download-dir DIR`, `--cache-path PATH` | Override `downloadDir` and `cachePath` |
| `--skip-startup-verify` | SQLite backend: do not check every cached file against its hash at startup |
| `--disable-rate-limit` | No flood control, for when every reader appears to come from one address (NAT, a proxy that hides it) |
| `--trust-proxy-headers` | Take the reader's address from `X-Forwarded-For` (behind a proxy you trust) |
| `--validate-config` | Load the configuration and exit |
| `-v`, `--version` | Print the build's commit and date |

The client responds to `SIGINT` and `SIGTERM` for graceful shutdown. Settings reload automatically via RPC command. The Prometheus metrics endpoint is available at `/metrics` for monitoring.

## Architecture

The library is in `src/Hath`. Effects are [polysemy](https://hackage.haskell.org/package/polysemy) effects.

- `Hath.Protocol`, `Hath.FileURI`, `Hath.Settings`, `Hath.Gallery`: the wire formats as pure functions (signatures, reply verification, file ids, settings, download lists).
- `Hath.RPC`: requests to the H@H RPC server.
- `Hath.Cache` and `Hath.Cache.{Filesystem,R2,SQLite}`: the cache effect and its backends.
- `Hath.Locate`: finds a requested file in the cache, or fetches it upstream and relays it as it arrives.
- `Hath.Server` and `Hath.Server.*`: the Warp/TLS server, its routes, handlers, middleware and flood control.
- `Hath.Gallery.Downloader`: the gallery downloader.
- `Hath.Stats` and `Hath.Metrics.*`: the counters and their Prometheus rendering.

`app/` holds the executables: `hs-hath` itself, `migrate-to-r2` (uploads an SQLite or Filesystem cache to a bucket) and `hath-bench` (plays the RPC server for a client under test and prints a scorecard).

## Metrics

The client exports Prometheus metrics in text format at the `/metrics` endpoint. Metrics include request counters, cache statistics, and operational counters for monitoring and alerting.

## Performance

Has not been optimized yet. Currently, it will take ~200MiB RAM on average.

## Troubleshooting

### Certificate errors
Verify system clock synchronization and certificate file permissions. Certificate refresh occurs automatically every 24 hours.

### Connection issues
The `proxy` field of the configuration is not used; outgoing requests follow the `http_proxy` and `https_proxy` environment variables, as `http-client` does by default. Ensure the firewall allows outbound connections to the RPC server (port 80) and to image servers, and inbound connections on the port set on the H@H settings page.

### Storage backend errors
For S3-compatible backends like Cloudflare R2, verify that the storage server returns required headers: `ETag`, `Content-Length`, and `Last-Modified`. Missing `ETag` headers cause write failures.

### Rate limiting
The client implements strict request throttling. Banned IPs and rate limit violations appear in console logs. Verify client configuration if legitimate requests are being rejected.

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.
