# hs-hath

A [Hentai@Home](https://ehwiki.org/wiki/Hentai@Home) client written in Haskell, also on [Hackage](https://hackage.haskell.org/package/hs-hath).

It caches files on the local disk, in an SQLite database, or in S3-compatible storage (such as Cloudflare R2), uses Dhall for configuration, and exposes Prometheus metrics at `/metrics`.

## Getting Started

### Prerequisites

* [Stack](https://docs.haskellstack.org/)
* SQLite 3
* The `openssl` command, only to run the test suite and `hath-bench`, which create certificates with it

### Installation

1. Build the project:
   ```sh
   stack build
   ```
2. Copy `client-login.example` to `client-login` and fill in your credentials and paths:
   ```sh
   cp client-login.example client-login
   ```
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
   The example defines the record's types and defaults, so the file only names what differs. `version`, `proxy` and `lruCacheSize` are not read. `cacheBackend` is one of:

   * `CacheBackendFilesystem`: files under `cachePath` (a directory) as `xxxx/<file id>`, sent straight from disk. The least recently served files are evicted to stay under the disk limit set on the H@H settings page.
   * `CacheBackendSQLite`: files stored in the database file at `cachePath`. Nothing is evicted, and every cached file is checked against its hash at start unless `--skip-startup-verify` is given.
   * `CacheBackendR2`: `r2Config = Some { r2Endpoint = "https://<account id>.r2.cloudflarestorage.com", r2Bucket = "<bucket>" }`, with `R2_ACCESS_KEY` and `R2_SECRET_KEY` in the environment. A hit is answered with a redirect to a presigned link; nothing is evicted.

## Usage

Run the client:

```sh
stack exec hs-hath
```

The configuration is read from `./client-login`; `-c FILE` names another file. `stack exec hs-hath -- --help` lists the other options.

Prometheus metrics are served at `/metrics`, and the settings the server assigned at `/admin/settings`, once `HATH_METRICS_TOKEN` is set in the environment (any long random string, for example from `openssl rand -hex 32`). A request must send it as `Authorization: Bearer <token>`; in Prometheus:

```yaml
scrape_configs:
  - job_name: hath
    scheme: https
    authorization:
      credentials: <token>
    static_configs:
      - targets: ["<host>:<port>"]
```

Without the variable both paths answer 404, like any path the official client does not have.

## Notes & Caveats

* **Memory usage**: Not optimized yet — expect ~200 MiB RAM on average.
* **S3 / Cloudflare R2**: The object store must return `ETag`, `Content-Length`, and `Last-Modified` headers; missing `ETag` will fail cache writes.
* **Reverse proxy**: Run with `--trust-proxy-headers`, which takes the reader's address from `X-Forwarded-For` when the proxy is on a loopback or private address. Flood control then counts readers, and server commands (`/servercmd/`) are taken from the RPC servers' addresses only. The connection limit sees only the peer's address and never refuses a local one, so it does not apply to a proxy on a local address.
* **Tunnels** (rathole, frp, a plain port forward that rewrites the source): hs-hath sees every connection come from a local address and cannot tell readers apart, so neither flood control nor the connection limit applies; limit connections where the tunnel meets the internet. A server command from a local address is accepted on its key alone.
* **Start-up**: If the server refuses `client_start` because another client is connected or the client id is in use, hs-hath exits. After any other failure (for example `FAIL_CONNECT_TEST`) it stays up without announcing itself, as the official client does, so that the port can be tested; restart it once the cause is fixed. A certificate with less than 24 h left is refused at start.
* **Galleries** are saved as `<downloadDir>/<title> [<gid>]`, or `[<gid>-<xres>x]` for a resized one, and `galleryinfo.txt` is written once all of a gallery's files are there.

## License

Distributed under the GPL-3.0 License. See [`LICENSE`](LICENSE).
