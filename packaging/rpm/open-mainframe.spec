Name:           open-mainframe
Version:        0.1.0
Release:        1%{?dist}
Summary:        Mainframe COBOL compiler and JCL interpreter

License:        Apache-2.0
URL:            https://github.com/toreleon/OpenMainframe
Source0:        %{name}-%{version}.tar.gz

BuildRequires:  rust >= 1.82
BuildRequires:  cargo
BuildRequires:  gcc

%description
OpenMainframe is a mainframe emulator that provides:
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
install -Dm755 target/release/open-mainframe %{buildroot}%{_bindir}/open-mainframe

# Man pages
install -Dm644 packaging/man/open-mainframe.1 %{buildroot}%{_mandir}/man1/open-mainframe.1
install -Dm644 packaging/man/open-mainframe-compile.1 %{buildroot}%{_mandir}/man1/open-mainframe-compile.1
install -Dm644 packaging/man/open-mainframe-run.1 %{buildroot}%{_mandir}/man1/open-mainframe-run.1
install -Dm644 packaging/man/open-mainframe-check.1 %{buildroot}%{_mandir}/man1/open-mainframe-check.1
install -Dm644 packaging/man/open-mainframe-gdg.1 %{buildroot}%{_mandir}/man1/open-mainframe-gdg.1
install -Dm644 packaging/man/open-mainframe-idcams.1 %{buildroot}%{_mandir}/man1/open-mainframe-idcams.1

# Shell completions
install -Dm644 packaging/completions/open-mainframe.bash %{buildroot}%{_datadir}/bash-completion/completions/open-mainframe
install -Dm644 packaging/completions/open-mainframe.zsh %{buildroot}%{_datadir}/zsh/site-functions/_open-mainframe
install -Dm644 packaging/completions/open-mainframe.fish %{buildroot}%{_datadir}/fish/vendor_completions.d/open-mainframe.fish

%check
cargo test

%files
%license LICENSE
%doc README.md
%{_bindir}/open-mainframe
%{_mandir}/man1/open-mainframe*.1*
%{_datadir}/bash-completion/completions/open-mainframe
%{_datadir}/zsh/site-functions/_open-mainframe
%{_datadir}/fish/vendor_completions.d/open-mainframe.fish

%changelog
* Thu Feb 13 2026 OpenMainframe Maintainers <maintainers@openmainframe.dev> - 0.1.0-1
- Initial release
- COBOL compiler with lexer, parser, semantic analysis
- LLVM code generation backend
- JCL interpreter with job/step execution
- Dataset support: sequential, VSAM (KSDS, ESDS, RRDS)
- GDG (Generation Data Group) management
- IDCAMS utility for catalog operations
- SORT/DFSORT utility
