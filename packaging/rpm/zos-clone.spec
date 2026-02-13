Name:           zos-clone
Version:        0.1.0
Release:        1%{?dist}
Summary:        Mainframe COBOL compiler and JCL interpreter

License:        Apache-2.0
URL:            https://github.com/zos-clone/zos-clone
Source0:        %{name}-%{version}.tar.gz

BuildRequires:  rust >= 1.82
BuildRequires:  cargo
BuildRequires:  gcc

%description
zOS-clone is a mainframe emulator that provides:
- COBOL compiler with LLVM backend
- JCL (Job Control Language) interpreter
- VSAM dataset support (KSDS, ESDS, RRDS)
- GDG (Generation Data Group) management
- IDCAMS utility for catalog operations
- SORT/DFSORT utility

This package enables running legacy mainframe workloads on modern Linux systems.

%prep
%autosetup

%build
cargo build --release

%install
install -Dm755 target/release/zos-clone %{buildroot}%{_bindir}/zos-clone

# Man pages
install -Dm644 packaging/man/zos-clone.1 %{buildroot}%{_mandir}/man1/zos-clone.1
install -Dm644 packaging/man/zos-clone-compile.1 %{buildroot}%{_mandir}/man1/zos-clone-compile.1
install -Dm644 packaging/man/zos-clone-run.1 %{buildroot}%{_mandir}/man1/zos-clone-run.1
install -Dm644 packaging/man/zos-clone-check.1 %{buildroot}%{_mandir}/man1/zos-clone-check.1
install -Dm644 packaging/man/zos-clone-gdg.1 %{buildroot}%{_mandir}/man1/zos-clone-gdg.1
install -Dm644 packaging/man/zos-clone-idcams.1 %{buildroot}%{_mandir}/man1/zos-clone-idcams.1

# Shell completions
install -Dm644 packaging/completions/zos-clone.bash %{buildroot}%{_datadir}/bash-completion/completions/zos-clone
install -Dm644 packaging/completions/zos-clone.zsh %{buildroot}%{_datadir}/zsh/site-functions/_zos-clone
install -Dm644 packaging/completions/zos-clone.fish %{buildroot}%{_datadir}/fish/vendor_completions.d/zos-clone.fish

%check
cargo test

%files
%license LICENSE
%doc README.md
%{_bindir}/zos-clone
%{_mandir}/man1/zos-clone*.1*
%{_datadir}/bash-completion/completions/zos-clone
%{_datadir}/zsh/site-functions/_zos-clone
%{_datadir}/fish/vendor_completions.d/zos-clone.fish

%changelog
* Thu Feb 13 2026 zOS-clone Maintainers <maintainers@zos-clone.dev> - 0.1.0-1
- Initial release
- COBOL compiler with lexer, parser, semantic analysis
- LLVM code generation backend
- JCL interpreter with job/step execution
- Dataset support: sequential, VSAM (KSDS, ESDS, RRDS)
- GDG (Generation Data Group) management
- IDCAMS utility for catalog operations
- SORT/DFSORT utility
