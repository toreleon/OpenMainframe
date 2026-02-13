#!/bin/bash
# Build script for creating distribution packages

set -e

VERSION="${VERSION:-0.1.0}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
BUILD_DIR="$ROOT_DIR/target/package"

usage() {
    echo "Usage: $0 [command]"
    echo ""
    echo "Commands:"
    echo "  completions   Generate shell completions"
    echo "  deb           Build Debian package"
    echo "  rpm           Build RPM package (requires rpmbuild)"
    echo "  all           Build all packages"
    echo "  clean         Clean build artifacts"
    echo ""
    echo "Environment variables:"
    echo "  VERSION       Package version (default: $VERSION)"
}

generate_completions() {
    echo "Generating shell completions..."
    mkdir -p "$SCRIPT_DIR/completions"

    cargo build --release

    "$ROOT_DIR/target/release/zos-clone" completions bash > "$SCRIPT_DIR/completions/zos-clone.bash"
    "$ROOT_DIR/target/release/zos-clone" completions zsh > "$SCRIPT_DIR/completions/zos-clone.zsh"
    "$ROOT_DIR/target/release/zos-clone" completions fish > "$SCRIPT_DIR/completions/zos-clone.fish"

    echo "Completions generated in $SCRIPT_DIR/completions/"
}

build_deb() {
    echo "Building Debian package..."

    if ! command -v dpkg-deb &> /dev/null; then
        echo "Error: dpkg-deb not found. Install dpkg-dev package."
        exit 1
    fi

    cargo build --release

    mkdir -p "$BUILD_DIR/deb/DEBIAN"
    mkdir -p "$BUILD_DIR/deb/usr/bin"
    mkdir -p "$BUILD_DIR/deb/usr/share/man/man1"
    mkdir -p "$BUILD_DIR/deb/usr/share/bash-completion/completions"
    mkdir -p "$BUILD_DIR/deb/usr/share/zsh/vendor-completions"
    mkdir -p "$BUILD_DIR/deb/usr/share/fish/vendor_completions.d"
    mkdir -p "$BUILD_DIR/deb/usr/share/doc/zos-clone"

    # Binary
    cp "$ROOT_DIR/target/release/zos-clone" "$BUILD_DIR/deb/usr/bin/"

    # Man pages
    cp "$SCRIPT_DIR/man/"*.1 "$BUILD_DIR/deb/usr/share/man/man1/"
    gzip -f -9 "$BUILD_DIR/deb/usr/share/man/man1/"*.1

    # Completions
    cp "$SCRIPT_DIR/completions/zos-clone.bash" "$BUILD_DIR/deb/usr/share/bash-completion/completions/zos-clone"
    cp "$SCRIPT_DIR/completions/zos-clone.zsh" "$BUILD_DIR/deb/usr/share/zsh/vendor-completions/_zos-clone"
    cp "$SCRIPT_DIR/completions/zos-clone.fish" "$BUILD_DIR/deb/usr/share/fish/vendor_completions.d/"

    # Documentation
    [ -f "$ROOT_DIR/README.md" ] && cp "$ROOT_DIR/README.md" "$BUILD_DIR/deb/usr/share/doc/zos-clone/"
    [ -f "$ROOT_DIR/LICENSE" ] && cp "$ROOT_DIR/LICENSE" "$BUILD_DIR/deb/usr/share/doc/zos-clone/copyright"

    # Control file
    cat > "$BUILD_DIR/deb/DEBIAN/control" << EOF
Package: zos-clone
Version: $VERSION
Section: devel
Priority: optional
Architecture: $(dpkg --print-architecture)
Depends: libc6 (>= 2.17)
Maintainer: zOS-clone Maintainers <maintainers@zos-clone.dev>
Description: Mainframe COBOL compiler and JCL interpreter
 zOS-clone is a mainframe emulator that provides COBOL compilation,
 JCL interpretation, and dataset management for running legacy
 mainframe workloads on modern Linux systems.
Homepage: https://github.com/zos-clone/zos-clone
EOF

    # Build package
    fakeroot dpkg-deb --build "$BUILD_DIR/deb" "$BUILD_DIR/zos-clone_${VERSION}_$(dpkg --print-architecture).deb"

    echo "Debian package built: $BUILD_DIR/zos-clone_${VERSION}_$(dpkg --print-architecture).deb"
}

build_rpm() {
    echo "Building RPM package..."

    if ! command -v rpmbuild &> /dev/null; then
        echo "Error: rpmbuild not found. Install rpm-build package."
        exit 1
    fi

    cargo build --release

    mkdir -p "$BUILD_DIR/rpm"/{BUILD,RPMS,SOURCES,SPECS,SRPMS}

    # Create tarball
    TARBALL_DIR="zos-clone-$VERSION"
    mkdir -p "$BUILD_DIR/rpm/$TARBALL_DIR"
    cp -r "$ROOT_DIR"/* "$BUILD_DIR/rpm/$TARBALL_DIR/" 2>/dev/null || true
    (cd "$BUILD_DIR/rpm" && tar czvf "SOURCES/$TARBALL_DIR.tar.gz" "$TARBALL_DIR")
    rm -rf "$BUILD_DIR/rpm/$TARBALL_DIR"

    # Update spec file version
    sed "s/^Version:.*/Version:        $VERSION/" "$SCRIPT_DIR/rpm/zos-clone.spec" > "$BUILD_DIR/rpm/SPECS/zos-clone.spec"

    # Build RPM
    rpmbuild -bb "$BUILD_DIR/rpm/SPECS/zos-clone.spec" \
        --define "_topdir $BUILD_DIR/rpm" \
        --define "debug_package %{nil}"

    # Copy result
    cp "$BUILD_DIR/rpm/RPMS/"*/*.rpm "$BUILD_DIR/"

    echo "RPM package built in $BUILD_DIR/"
}

clean() {
    echo "Cleaning build artifacts..."
    rm -rf "$BUILD_DIR"
    echo "Done."
}

case "${1:-}" in
    completions)
        generate_completions
        ;;
    deb)
        build_deb
        ;;
    rpm)
        build_rpm
        ;;
    all)
        generate_completions
        build_deb
        build_rpm
        ;;
    clean)
        clean
        ;;
    *)
        usage
        exit 1
        ;;
esac
