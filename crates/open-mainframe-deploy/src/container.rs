//! Container image building support.
//!
//! Generates multi-stage Dockerfiles for optimized production container
//! images with minimal runtime dependencies.

use serde::Serialize;

/// Configuration for Dockerfile generation.
#[derive(Debug, Clone, Serialize)]
pub struct DockerConfig {
    /// Rust toolchain version for the build stage.
    pub rust_version: String,
    /// Base image for the runtime stage.
    pub runtime_base: String,
    /// Binary name to build and copy.
    pub binary_name: String,
    /// Exposed port for the application.
    pub app_port: u16,
    /// Exposed port for metrics.
    pub metrics_port: u16,
    /// Additional Cargo features to enable.
    pub features: Vec<String>,
    /// Whether to include health-check instruction.
    pub include_healthcheck: bool,
    /// Labels to add to the image.
    pub labels: Vec<(String, String)>,
}

impl Default for DockerConfig {
    fn default() -> Self {
        Self {
            rust_version: "1.82".to_string(),
            runtime_base: "debian:bookworm-slim".to_string(),
            binary_name: "open-mainframe".to_string(),
            app_port: 8080,
            metrics_port: 9090,
            features: Vec::new(),
            include_healthcheck: true,
            labels: vec![
                ("org.opencontainers.image.title".to_string(), "OpenMainframe".to_string()),
                ("org.opencontainers.image.description".to_string(), "Mainframe application runtime".to_string()),
            ],
        }
    }
}

/// Generate a multi-stage Dockerfile.
///
/// The Dockerfile uses a builder stage to compile the Rust binary and
/// a minimal runtime stage with only the compiled binary and runtime
/// dependencies (libssl, ca-certificates).
pub fn generate_dockerfile(config: &DockerConfig) -> String {
    let features_flag = if config.features.is_empty() {
        String::new()
    } else {
        format!(" --features {}", config.features.join(","))
    };

    let labels = config
        .labels
        .iter()
        .map(|(k, v)| format!("LABEL {}=\"{}\"", k, v))
        .collect::<Vec<_>>()
        .join("\n");

    let healthcheck = if config.include_healthcheck {
        format!(
            "\nHEALTHCHECK --interval=10s --timeout=3s --retries=3 \\\n  CMD curl -f http://localhost:{}/health || exit 1",
            config.app_port
        )
    } else {
        String::new()
    };

    format!(
        r#"# Build stage: compile the Rust binary
FROM rust:{rust_version} AS builder

WORKDIR /build

# Cache dependencies by copying manifests first
COPY Cargo.toml Cargo.lock ./
COPY crates/ crates/

# Build release binary
RUN cargo build --release{features_flag} \
    && strip target/release/{binary_name}

# Runtime stage: minimal image with just the binary
FROM {runtime_base}

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       ca-certificates \
       libssl3 \
       curl \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN groupadd -r omf && useradd -r -g omf -d /app omf

WORKDIR /app

# Copy binary from builder
COPY --from=builder /build/target/release/{binary_name} /app/{binary_name}

# Create secrets mount point
RUN mkdir -p /etc/open-mainframe/secrets && chown omf:omf /etc/open-mainframe/secrets

{labels}

EXPOSE {app_port} {metrics_port}
{healthcheck}

USER omf

ENTRYPOINT ["/app/{binary_name}"]
"#,
        rust_version = config.rust_version,
        features_flag = features_flag,
        binary_name = config.binary_name,
        runtime_base = config.runtime_base,
        app_port = config.app_port,
        metrics_port = config.metrics_port,
        labels = labels,
        healthcheck = healthcheck,
    )
}

/// Generate a .dockerignore file to keep the build context small.
pub fn generate_dockerignore() -> String {
    r#"target/
.git/
.gitignore
*.md
LICENSE
.env
.vscode/
.idea/
*.log
"#
    .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_dockerfile() {
        let config = DockerConfig::default();
        let dockerfile = generate_dockerfile(&config);
        assert!(dockerfile.contains("FROM rust:1.82 AS builder"));
        assert!(dockerfile.contains("FROM debian:bookworm-slim"));
        assert!(dockerfile.contains("cargo build --release"));
        assert!(dockerfile.contains("strip target/release/open-mainframe"));
        assert!(dockerfile.contains("EXPOSE 8080 9090"));
        assert!(dockerfile.contains("USER omf"));
    }

    #[test]
    fn test_multi_stage_build() {
        let config = DockerConfig::default();
        let dockerfile = generate_dockerfile(&config);
        // Must have exactly 2 FROM instructions (multi-stage)
        let from_count = dockerfile.lines().filter(|l| l.starts_with("FROM ")).count();
        assert_eq!(from_count, 2);
    }

    #[test]
    fn test_no_build_tools_in_runtime() {
        let config = DockerConfig::default();
        let dockerfile = generate_dockerfile(&config);
        // The runtime stage should not install cargo/rustc
        let runtime_section = dockerfile.split("FROM debian").nth(1).unwrap();
        assert!(!runtime_section.contains("cargo"));
        assert!(!runtime_section.contains("rustc"));
        assert!(!runtime_section.contains("rustup"));
    }

    #[test]
    fn test_non_root_user() {
        let config = DockerConfig::default();
        let dockerfile = generate_dockerfile(&config);
        assert!(dockerfile.contains("groupadd -r omf"));
        assert!(dockerfile.contains("useradd -r -g omf"));
        assert!(dockerfile.contains("USER omf"));
    }

    #[test]
    fn test_healthcheck_included() {
        let config = DockerConfig::default();
        let dockerfile = generate_dockerfile(&config);
        assert!(dockerfile.contains("HEALTHCHECK"));
        assert!(dockerfile.contains("curl -f http://localhost:8080/health"));
    }

    #[test]
    fn test_healthcheck_disabled() {
        let config = DockerConfig {
            include_healthcheck: false,
            ..Default::default()
        };
        let dockerfile = generate_dockerfile(&config);
        assert!(!dockerfile.contains("HEALTHCHECK"));
    }

    #[test]
    fn test_custom_features() {
        let config = DockerConfig {
            features: vec!["otel".to_string(), "tls".to_string()],
            ..Default::default()
        };
        let dockerfile = generate_dockerfile(&config);
        assert!(dockerfile.contains("--features otel,tls"));
    }

    #[test]
    fn test_custom_ports() {
        let config = DockerConfig {
            app_port: 3000,
            metrics_port: 3001,
            ..Default::default()
        };
        let dockerfile = generate_dockerfile(&config);
        assert!(dockerfile.contains("EXPOSE 3000 3001"));
    }

    #[test]
    fn test_labels() {
        let config = DockerConfig::default();
        let dockerfile = generate_dockerfile(&config);
        assert!(dockerfile.contains("LABEL org.opencontainers.image.title=\"OpenMainframe\""));
    }

    #[test]
    fn test_secrets_mount_point() {
        let config = DockerConfig::default();
        let dockerfile = generate_dockerfile(&config);
        assert!(dockerfile.contains("/etc/open-mainframe/secrets"));
    }

    #[test]
    fn test_dockerignore() {
        let ignore = generate_dockerignore();
        assert!(ignore.contains("target/"));
        assert!(ignore.contains(".git/"));
        assert!(ignore.contains("*.md"));
    }

    #[test]
    fn test_copy_from_builder() {
        let config = DockerConfig::default();
        let dockerfile = generate_dockerfile(&config);
        assert!(dockerfile.contains("COPY --from=builder"));
    }
}
