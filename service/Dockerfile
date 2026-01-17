FROM ubuntu:24.04

WORKDIR /

# 1. Install compatibility libraries
# If your binary was compiled on Ubuntu/Debian, it likely needs 'libc6-compat'.
# If your binary is statically linked (e.g. Go with CGO_ENABLED=0), you can remove this line.
RUN apt-get update && apt-get install -y \
    libgmp10 \
    libnuma1 \
    libtinfo6 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# 2. THE FIX: Create the 'libtinfow' alias manually
# We point 'libtinfow.so.6' to the existing 'libtinfo.so.6'
RUN ln -s /usr/lib/x86_64-linux-gnu/libtinfo.so.6 /usr/lib/x86_64-linux-gnu/libtinfow.so.6

# 2. Copy your binary
# Replace 'my-binary-name' with the actual filename
COPY hs-hath /hath

# 3. Make it executable
RUN chmod +x /hath

# 4. Run it
# Replace '3000' with the port your binary listens on
EXPOSE 443
ENTRYPOINT ["/hath"]
