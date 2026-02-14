# OpenMainframe Production Dockerfile
# Multi-stage build for minimal, secure container image

# =============================================================================
# Stage 1: Build
# =============================================================================
FROM rust:1.75-slim-bookworm AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Copy dependency files first for better layer caching
COPY Cargo.toml Cargo.lock ./
COPY crates/open-mainframe-*/Cargo.toml ./crates/

# Create stub files to compile dependencies
RUN mkdir -p src && echo "fn main() {}" > src/main.rs
RUN for dir in crates/*/; do \
    mkdir -p "$dir/src" && \
    echo "pub fn placeholder() {}" > "$dir/src/lib.rs"; \
    done

# Build dependencies only
RUN cargo build --release --workspace 2>/dev/null || true

# Copy actual source code
COPY . .

# Touch files to invalidate cache and rebuild with real sources
RUN find . -name "*.rs" -exec touch {} \;

# Build the release binary
RUN cargo build --release --workspace

# =============================================================================
# Stage 2: Runtime
# =============================================================================
FROM debian:bookworm-slim AS runtime

# Install runtime dependencies only
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libssl3 \
    libpq5 \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /var/cache/apt/*

# Create non-root user
RUN groupadd -r openmainframe -g 1000 && \
    useradd -r -g openmainframe -u 1000 -d /app -s /sbin/nologin openmainframe

WORKDIR /app

# Copy binaries from builder
COPY --from=builder /build/target/release/open-mainframe-* /usr/local/bin/

# Create required directories
RUN mkdir -p /etc/open-mainframe /var/log/open-mainframe /tmp/open-mainframe && \
    chown -R openmainframe:openmainframe /etc/open-mainframe /var/log/open-mainframe /tmp/open-mainframe

# Copy configuration template
COPY --from=builder /build/deploy/kubernetes/configmap.yaml /etc/open-mainframe/

# Switch to non-root user
USER openmainframe

# Environment variables
ENV OPEN_MAINFRAME_LOG_LEVEL=info \
    OPEN_MAINFRAME_LOG_FORMAT=json \
    OPEN_MAINFRAME_SERVER_HOST=0.0.0.0 \
    OPEN_MAINFRAME_SERVER_PORT=8080 \
    OPEN_MAINFRAME_METRICS_PORT=9090

# Expose ports
EXPOSE 8080 9090

# Health check
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

# Default command - can be overridden
CMD ["open-mainframe-runtime"]

# Labels
LABEL org.opencontainers.image.title="OpenMainframe" \
      org.opencontainers.image.description="Mainframe-compatible runtime for COBOL, CICS, and IMS" \
      org.opencontainers.image.version="1.3.0" \
      org.opencontainers.image.vendor="OpenMainframe Team" \
      org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.source="https://github.com/toreleon/OpenMainframe"
