FROM ubuntu:24.04

WORKDIR /

RUN apt-get update && apt-get install -y \
    libgmp10 \
    libnuma1 \
    libtinfo6 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

RUN ln -s /usr/lib/x86_64-linux-gnu/libtinfo.so.6 /usr/lib/x86_64-linux-gnu/libtinfow.so.6

COPY hs-hath /hath

RUN chmod +x /hath

EXPOSE 443
ENTRYPOINT ["/hath"]
