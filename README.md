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

It tracks cached files in an SQLite LRU index with support for both local disk and S3-compatible storage (such as Cloudflare R2), uses Dhall for configuration, and exposes Prometheus metrics at `/metrics`.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

### Built With

* [![Haskell][Haskell-badge]][Haskell-url]
* [![SQLite][SQLite-badge]][SQLite-url]
* [![Dhall][Dhall-badge]][Dhall-url]
* [![OpenSSL][OpenSSL-badge]][OpenSSL-url]
* [![Prometheus][Prometheus-badge]][Prometheus-url]

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Getting Started

### Prerequisites

* [Stack](https://docs.haskellstack.org/)
* SQLite 3
* OpenSSL (development headers/libraries)

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
   { clientId = "your-client-id"
   , key = "your-api-key"
   , version = "1.0.0"
   , proxy = None ClientProxy
   , downloadDir = "./downloads"
   , cachePath = "./cache.db"
   }
   ```

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

Prometheus metrics are served at `/metrics`.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Notes & Caveats

* **Memory usage**: Not optimized yet — expect ~200 MiB RAM on average.
* **S3 / Cloudflare R2**: The object store must return `ETag`, `Content-Length`, and `Last-Modified` headers; missing `ETag` will fail cache writes.

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
[OpenSSL-badge]: https://img.shields.io/badge/OpenSSL-721412?style=for-the-badge&logo=openssl&logoColor=white
[OpenSSL-url]: https://www.openssl.org/
[Prometheus-badge]: https://img.shields.io/badge/Prometheus-e6522c?style=for-the-badge&logo=prometheus&logoColor=white
[Prometheus-url]: https://prometheus.io/
