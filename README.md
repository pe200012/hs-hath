# hs-hath - A Haskell Implementation of the Hentai@Home Client

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

A high-performance Haskell implementation of the Hentai@Home client, featuring:

- Efficient resource caching and serving
- TLS-secured RPC communication
- Rate limiting and flood control
- Gallery download capabilities
- Automatic certificate management

## Table of Contents
- [Features](#features)
- [Installation](#installation)
- [Configuration](#configuration)
- [Usage](#usage)  
- [Architecture](#architecture)
- [Performance](#performance)
- [Troubleshooting](#troubleshooting)
- [Contributing](#contributing)
- [License](#license)

## Features

- **Resource Caching**: Smart LRU-based caching of served resources
- **RPC Communication**: Secure client-server communication via TLS
- **Rate Limiting**: Built-in request throttling and IP banning
- **Gallery Downloads**: Parallel downloading of gallery files
- **Automatic Updates**: Certificate and settings refresh
- **Logging**: Detailed operational logging

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

## Usage

Start the client:
```bash
stack exec hs-hath
```

Key commands:
- `SIGINT`/`SIGTERM`: Graceful shutdown
- Automatic certificate refresh every 24 hours
- Settings reload via RPC command

## Architecture

The system consists of several key components:

1. **RPC Client**: Handles communication with the H@H server
2. **Resource Cache**: SQLite-backed file storage
3. **HTTP Server**: Warp-based web server for resource delivery
4. **Rate Limiter**: Request throttling middleware
5. **Task Manager**: Gallery download coordination

## Performance

The client is optimized for:
- High concurrent request handling
- Minimal memory footprint (~50MB baseline)
- Efficient disk caching
- Low-latency responses

## Troubleshooting

Common issues:

**Certificate errors**:
- Verify system clock is synchronized
- Check certificate file permissions

**Connection issues**:
- Verify proxy settings if used
- Check firewall allows outbound TLS connections

**Rate limiting**:
- Client implements strict request throttling
- Banned IPs are logged in the console

## Contributing

Contributions welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Submit a pull request

Ensure all changes:
- Maintain backwards compatibility
- Include tests where applicable
- Follow existing code style

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.
