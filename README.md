<a id="readme-top"></a>

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![GPL-3.0 License][license-shield]][license-url]

<br />
<div align="center">
  <h3 align="center">hs-hath</h3>

  <p align="center">
    A Hentai@Home client written in Haskell.
    <br />
    <a href="https://hackage.haskell.org/package/hs-hath"><strong>Hackage »</strong></a>
    <br />
    <br />
    <a href="https://github.com/pe200012/hs-hath/issues/new?labels=bug">Report Bug</a>
    &middot;
    <a href="https://github.com/pe200012/hs-hath/issues/new?labels=enhancement">Request Feature</a>
  </p>
</div>

<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#notes--caveats">Notes &amp; Caveats</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>

## About The Project

`hs-hath` is a Hentai@Home client written in Haskell.

It caches files on the local disk, in an SQLite database, or in S3-compatible storage (such as Cloudflare R2), uses Dhall for configuration, and exposes Prometheus metrics at `/metrics`.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

### Built With

* [![Haskell][Haskell-badge]][Haskell-url]
* [![SQLite][SQLite-badge]][SQLite-url]
* [![Dhall][Dhall-badge]][Dhall-url]
* [![Prometheus][Prometheus-badge]][Prometheus-url]

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Getting Started

### Prerequisites

* [Stack](https://docs.haskellstack.org/)
* SQLite 3
* The `openssl` command, only to run the test suite and `hath-bench`, which create certificates with it

### Installation

1. Clone the repository:
   ```sh
   git clone https://github.com/pe200012/hs-hath.git
   cd hs-hath
   ```
2. Build the project:
   ```sh
   stack build
   ```
3. Copy `client-login.example` to `client-login` and fill in your credentials and paths:
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

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Usage

Run the client:

```sh
stack exec hs-hath
```

Check version and build metadata:

```sh
stack exec hs-hath -- --version
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

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Notes & Caveats

* **Memory usage**: Not optimized yet — expect ~200 MiB RAM on average.
* **S3 / Cloudflare R2**: The object store must return `ETag`, `Content-Length`, and `Last-Modified` headers; missing `ETag` will fail cache writes.
* **Reverse proxy**: Run with `--trust-proxy-headers`, which takes the reader's address from `X-Forwarded-For` when the proxy is on a loopback or private address. Flood control then counts readers, and server commands (`/servercmd/`) are taken from the RPC servers' addresses only. The connection limit sees only the peer's address and never refuses a local one, so it does not apply to a proxy on a local address.
* **Tunnels** (rathole, frp, a plain port forward that rewrites the source): hs-hath sees every connection come from a local address and cannot tell readers apart, so neither flood control nor the connection limit applies; limit connections where the tunnel meets the internet. A server command from a local address is accepted on its key alone.
* **Start-up**: If the server refuses `client_start` because another client is connected or the client id is in use, hs-hath exits. After any other failure (for example `FAIL_CONNECT_TEST`) it stays up without announcing itself, as the official client does, so that the port can be tested; restart it once the cause is fixed. A certificate with less than 24 h left is refused at start.
* **Galleries** are saved as `<downloadDir>/<title> [<gid>]`, or `[<gid>-<xres>x]` for a resized one, and `galleryinfo.txt` is written once all of a gallery's files are there.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Roadmap

- [ ] Memory footprint optimization (currently ~200 MiB RAM on average)

See the [open issues][issues-url] for a full list of proposed features and known issues.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Contributing

Contributions are welcome. Feel free to fork the repository and open a pull request, or open an issue.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

### Top contributors

<a href="https://github.com/pe200012/hs-hath/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=pe200012/hs-hath" alt="contrib.rocks image" />
</a>

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## License

Distributed under the GPL-3.0 License. See [`LICENSE`][license-url] for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Contact

pe200012 - [@pe200012](https://github.com/pe200012)

Project Link: [https://github.com/pe200012/hs-hath](https://github.com/pe200012/hs-hath)

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Acknowledgments

* [Hentai@Home](https://ehwiki.org/wiki/Hentai@Home)
* [Best-README-Template](https://github.com/othneildrew/Best-README-Template)

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- MARKDOWN LINKS & IMAGES -->
[contributors-shield]: https://img.shields.io/github/contributors/pe200012/hs-hath.svg?style=for-the-badge
[contributors-url]: https://github.com/pe200012/hs-hath/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/pe200012/hs-hath.svg?style=for-the-badge
[forks-url]: https://github.com/pe200012/hs-hath/network/members
[stars-shield]: https://img.shields.io/github/stars/pe200012/hs-hath.svg?style=for-the-badge
[stars-url]: https://github.com/pe200012/hs-hath/stargazers
[issues-shield]: https://img.shields.io/github/issues/pe200012/hs-hath.svg?style=for-the-badge
[issues-url]: https://github.com/pe200012/hs-hath/issues
[license-shield]: https://img.shields.io/github/license/pe200012/hs-hath.svg?style=for-the-badge
[license-url]: https://github.com/pe200012/hs-hath/blob/HEAD/LICENSE
[Haskell-badge]: https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white
[Haskell-url]: https://www.haskell.org/
[SQLite-badge]: https://img.shields.io/badge/SQLite-07405e?style=for-the-badge&logo=sqlite&logoColor=white
[SQLite-url]: https://www.sqlite.org/
[Dhall-badge]: https://img.shields.io/badge/Dhall-222222?style=for-the-badge
[Dhall-url]: https://dhall-lang.org/
[Prometheus-badge]: https://img.shields.io/badge/Prometheus-e6522c?style=for-the-badge&logo=prometheus&logoColor=white
[Prometheus-url]: https://prometheus.io/
