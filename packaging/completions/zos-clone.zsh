#compdef zos-clone

autoload -U is-at-least

_zos-clone() {
    typeset -A opt_args
    typeset -a _arguments_options
    local ret=1

    if is-at-least 5.2; then
        _arguments_options=(-s -S -C)
    else
        _arguments_options=(-s -C)
    fi

    local context curcontext="$curcontext" state line
    _arguments "${_arguments_options[@]}" : \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
":: :_zos-clone_commands" \
"*::: :->zos-clone" \
&& ret=0
    case $state in
    (zos-clone)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:zos-clone-command-$line[1]:"
        case $line[1] in
            (compile)
_arguments "${_arguments_options[@]}" : \
'-o+[Output file path]:FILE:_files' \
'--output=[Output file path]:FILE:_files' \
'-O+[Optimization level (0-3)]:OPTIMIZE:_default' \
'--optimize=[Optimization level (0-3)]:OPTIMIZE:_default' \
'*-I+[Additional copybook search paths]:DIR:_files' \
'*--include=[Additional copybook search paths]:DIR:_files' \
'--emit-asm[Emit assembly instead of object file]' \
'--emit-llvm[Emit LLVM IR instead of object file]' \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
':input -- Input COBOL source file:_files' \
&& ret=0
;;
(run)
_arguments "${_arguments_options[@]}" : \
'--program-dir=[Directory containing compiled programs]:DIR:_files' \
'--dataset-dir=[Directory for dataset files]:DIR:_files' \
'--work-dir=[Working directory for job execution]:DIR:_files' \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
':input -- Input JCL file:_files' \
&& ret=0
;;
(check)
_arguments "${_arguments_options[@]}" : \
'*-I+[Additional copybook search paths]:DIR:_files' \
'*--include=[Additional copybook search paths]:DIR:_files' \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
':input -- Input COBOL source file:_files' \
&& ret=0
;;
(parse-jcl)
_arguments "${_arguments_options[@]}" : \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
':input -- Input JCL file:_files' \
&& ret=0
;;
(lex)
_arguments "${_arguments_options[@]}" : \
'--format=[Source format (fixed, free, or auto)]:FORMAT:_default' \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
':input -- Input COBOL source file:_files' \
&& ret=0
;;
(interpret)
_arguments "${_arguments_options[@]}" : \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
':input -- Input COBOL source file:_files' \
&& ret=0
;;
(gdg)
_arguments "${_arguments_options[@]}" : \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
":: :_zos-clone__gdg_commands" \
"*::: :->gdg" \
&& ret=0

    case $state in
    (gdg)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:zos-clone-gdg-command-$line[1]:"
        case $line[1] in
            (create)
_arguments "${_arguments_options[@]}" : \
'-l+[Maximum number of generations to keep (1-255)]:LIMIT:_default' \
'--limit=[Maximum number of generations to keep (1-255)]:LIMIT:_default' \
'--dataset-dir=[Base directory for datasets]:DATASET_DIR:_files' \
'--scratch[Delete old generations when rolling off (default\: true)]' \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
':name -- GDG base name (e.g., MY.GDG.BASE):_default' \
&& ret=0
;;
(list)
_arguments "${_arguments_options[@]}" : \
'--dataset-dir=[Base directory for datasets]:DATASET_DIR:_files' \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
':name -- GDG base name:_default' \
&& ret=0
;;
(delete)
_arguments "${_arguments_options[@]}" : \
'--dataset-dir=[Base directory for datasets]:DATASET_DIR:_files' \
'-f[Force deletion even if generations exist]' \
'--force[Force deletion even if generations exist]' \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
':name -- GDG base name:_default' \
&& ret=0
;;
(new-gen)
_arguments "${_arguments_options[@]}" : \
'--dataset-dir=[Base directory for datasets]:DATASET_DIR:_files' \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
':name -- GDG base name:_default' \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
":: :_zos-clone__gdg__help_commands" \
"*::: :->help" \
&& ret=0

    case $state in
    (help)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:zos-clone-gdg-help-command-$line[1]:"
        case $line[1] in
            (create)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(list)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(delete)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(new-gen)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
        esac
    ;;
esac
;;
        esac
    ;;
esac
;;
(idcams)
_arguments "${_arguments_options[@]}" : \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
":: :_zos-clone__idcams_commands" \
"*::: :->idcams" \
&& ret=0

    case $state in
    (idcams)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:zos-clone-idcams-command-$line[1]:"
        case $line[1] in
            (run)
_arguments "${_arguments_options[@]}" : \
'--dataset-dir=[Base directory for datasets]:DATASET_DIR:_files' \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(exec)
_arguments "${_arguments_options[@]}" : \
'--dataset-dir=[Base directory for datasets]:DATASET_DIR:_files' \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
':command -- The IDCAMS command to execute:_default' \
&& ret=0
;;
(file)
_arguments "${_arguments_options[@]}" : \
'--dataset-dir=[Base directory for datasets]:DATASET_DIR:_files' \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
':input -- Input file containing IDCAMS commands:_files' \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
":: :_zos-clone__idcams__help_commands" \
"*::: :->help" \
&& ret=0

    case $state in
    (help)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:zos-clone-idcams-help-command-$line[1]:"
        case $line[1] in
            (run)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(exec)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(file)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
        esac
    ;;
esac
;;
        esac
    ;;
esac
;;
(config)
_arguments "${_arguments_options[@]}" : \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
":: :_zos-clone__config_commands" \
"*::: :->config" \
&& ret=0

    case $state in
    (config)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:zos-clone-config-command-$line[1]:"
        case $line[1] in
            (show)
_arguments "${_arguments_options[@]}" : \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(init)
_arguments "${_arguments_options[@]}" : \
'-o+[Output path for config file]:OUTPUT:_files' \
'--output=[Output path for config file]:OUTPUT:_files' \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(paths)
_arguments "${_arguments_options[@]}" : \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
":: :_zos-clone__config__help_commands" \
"*::: :->help" \
&& ret=0

    case $state in
    (help)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:zos-clone-config-help-command-$line[1]:"
        case $line[1] in
            (show)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(init)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(paths)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
        esac
    ;;
esac
;;
        esac
    ;;
esac
;;
(completions)
_arguments "${_arguments_options[@]}" : \
'-v[Enable verbose output]' \
'--verbose[Enable verbose output]' \
'-h[Print help]' \
'--help[Print help]' \
':shell -- Shell to generate completions for:(bash elvish fish powershell zsh)' \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
":: :_zos-clone__help_commands" \
"*::: :->help" \
&& ret=0

    case $state in
    (help)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:zos-clone-help-command-$line[1]:"
        case $line[1] in
            (compile)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(run)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(check)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(parse-jcl)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(lex)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(interpret)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(gdg)
_arguments "${_arguments_options[@]}" : \
":: :_zos-clone__help__gdg_commands" \
"*::: :->gdg" \
&& ret=0

    case $state in
    (gdg)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:zos-clone-help-gdg-command-$line[1]:"
        case $line[1] in
            (create)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(list)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(delete)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(new-gen)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
        esac
    ;;
esac
;;
(idcams)
_arguments "${_arguments_options[@]}" : \
":: :_zos-clone__help__idcams_commands" \
"*::: :->idcams" \
&& ret=0

    case $state in
    (idcams)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:zos-clone-help-idcams-command-$line[1]:"
        case $line[1] in
            (run)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(exec)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(file)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
        esac
    ;;
esac
;;
(config)
_arguments "${_arguments_options[@]}" : \
":: :_zos-clone__help__config_commands" \
"*::: :->config" \
&& ret=0

    case $state in
    (config)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:zos-clone-help-config-command-$line[1]:"
        case $line[1] in
            (show)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(init)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(paths)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
        esac
    ;;
esac
;;
(completions)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" : \
&& ret=0
;;
        esac
    ;;
esac
;;
        esac
    ;;
esac
}

(( $+functions[_zos-clone_commands] )) ||
_zos-clone_commands() {
    local commands; commands=(
'compile:Compile a COBOL source file' \
'run:Run a JCL job' \
'check:Check COBOL syntax without compiling' \
'parse-jcl:Parse JCL and show the job structure' \
'lex:Show COBOL tokens (for debugging)' \
'interpret:Interpret a COBOL program (run without compiling)' \
'gdg:Manage Generation Data Groups (GDG)' \
'idcams:Run IDCAMS commands for dataset management' \
'config:Manage configuration' \
'completions:Generate shell completions' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'zos-clone commands' commands "$@"
}
(( $+functions[_zos-clone__check_commands] )) ||
_zos-clone__check_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone check commands' commands "$@"
}
(( $+functions[_zos-clone__compile_commands] )) ||
_zos-clone__compile_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone compile commands' commands "$@"
}
(( $+functions[_zos-clone__completions_commands] )) ||
_zos-clone__completions_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone completions commands' commands "$@"
}
(( $+functions[_zos-clone__config_commands] )) ||
_zos-clone__config_commands() {
    local commands; commands=(
'show:Show current configuration' \
'init:Generate a default configuration file' \
'paths:Show configuration file paths' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'zos-clone config commands' commands "$@"
}
(( $+functions[_zos-clone__config__help_commands] )) ||
_zos-clone__config__help_commands() {
    local commands; commands=(
'show:Show current configuration' \
'init:Generate a default configuration file' \
'paths:Show configuration file paths' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'zos-clone config help commands' commands "$@"
}
(( $+functions[_zos-clone__config__help__help_commands] )) ||
_zos-clone__config__help__help_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone config help help commands' commands "$@"
}
(( $+functions[_zos-clone__config__help__init_commands] )) ||
_zos-clone__config__help__init_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone config help init commands' commands "$@"
}
(( $+functions[_zos-clone__config__help__paths_commands] )) ||
_zos-clone__config__help__paths_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone config help paths commands' commands "$@"
}
(( $+functions[_zos-clone__config__help__show_commands] )) ||
_zos-clone__config__help__show_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone config help show commands' commands "$@"
}
(( $+functions[_zos-clone__config__init_commands] )) ||
_zos-clone__config__init_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone config init commands' commands "$@"
}
(( $+functions[_zos-clone__config__paths_commands] )) ||
_zos-clone__config__paths_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone config paths commands' commands "$@"
}
(( $+functions[_zos-clone__config__show_commands] )) ||
_zos-clone__config__show_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone config show commands' commands "$@"
}
(( $+functions[_zos-clone__gdg_commands] )) ||
_zos-clone__gdg_commands() {
    local commands; commands=(
'create:Create a new GDG base' \
'list:List GDG generations' \
'delete:Delete a GDG base and all generations' \
'new-gen:Create a new generation' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'zos-clone gdg commands' commands "$@"
}
(( $+functions[_zos-clone__gdg__create_commands] )) ||
_zos-clone__gdg__create_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone gdg create commands' commands "$@"
}
(( $+functions[_zos-clone__gdg__delete_commands] )) ||
_zos-clone__gdg__delete_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone gdg delete commands' commands "$@"
}
(( $+functions[_zos-clone__gdg__help_commands] )) ||
_zos-clone__gdg__help_commands() {
    local commands; commands=(
'create:Create a new GDG base' \
'list:List GDG generations' \
'delete:Delete a GDG base and all generations' \
'new-gen:Create a new generation' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'zos-clone gdg help commands' commands "$@"
}
(( $+functions[_zos-clone__gdg__help__create_commands] )) ||
_zos-clone__gdg__help__create_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone gdg help create commands' commands "$@"
}
(( $+functions[_zos-clone__gdg__help__delete_commands] )) ||
_zos-clone__gdg__help__delete_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone gdg help delete commands' commands "$@"
}
(( $+functions[_zos-clone__gdg__help__help_commands] )) ||
_zos-clone__gdg__help__help_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone gdg help help commands' commands "$@"
}
(( $+functions[_zos-clone__gdg__help__list_commands] )) ||
_zos-clone__gdg__help__list_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone gdg help list commands' commands "$@"
}
(( $+functions[_zos-clone__gdg__help__new-gen_commands] )) ||
_zos-clone__gdg__help__new-gen_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone gdg help new-gen commands' commands "$@"
}
(( $+functions[_zos-clone__gdg__list_commands] )) ||
_zos-clone__gdg__list_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone gdg list commands' commands "$@"
}
(( $+functions[_zos-clone__gdg__new-gen_commands] )) ||
_zos-clone__gdg__new-gen_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone gdg new-gen commands' commands "$@"
}
(( $+functions[_zos-clone__help_commands] )) ||
_zos-clone__help_commands() {
    local commands; commands=(
'compile:Compile a COBOL source file' \
'run:Run a JCL job' \
'check:Check COBOL syntax without compiling' \
'parse-jcl:Parse JCL and show the job structure' \
'lex:Show COBOL tokens (for debugging)' \
'interpret:Interpret a COBOL program (run without compiling)' \
'gdg:Manage Generation Data Groups (GDG)' \
'idcams:Run IDCAMS commands for dataset management' \
'config:Manage configuration' \
'completions:Generate shell completions' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'zos-clone help commands' commands "$@"
}
(( $+functions[_zos-clone__help__check_commands] )) ||
_zos-clone__help__check_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help check commands' commands "$@"
}
(( $+functions[_zos-clone__help__compile_commands] )) ||
_zos-clone__help__compile_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help compile commands' commands "$@"
}
(( $+functions[_zos-clone__help__completions_commands] )) ||
_zos-clone__help__completions_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help completions commands' commands "$@"
}
(( $+functions[_zos-clone__help__config_commands] )) ||
_zos-clone__help__config_commands() {
    local commands; commands=(
'show:Show current configuration' \
'init:Generate a default configuration file' \
'paths:Show configuration file paths' \
    )
    _describe -t commands 'zos-clone help config commands' commands "$@"
}
(( $+functions[_zos-clone__help__config__init_commands] )) ||
_zos-clone__help__config__init_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help config init commands' commands "$@"
}
(( $+functions[_zos-clone__help__config__paths_commands] )) ||
_zos-clone__help__config__paths_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help config paths commands' commands "$@"
}
(( $+functions[_zos-clone__help__config__show_commands] )) ||
_zos-clone__help__config__show_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help config show commands' commands "$@"
}
(( $+functions[_zos-clone__help__gdg_commands] )) ||
_zos-clone__help__gdg_commands() {
    local commands; commands=(
'create:Create a new GDG base' \
'list:List GDG generations' \
'delete:Delete a GDG base and all generations' \
'new-gen:Create a new generation' \
    )
    _describe -t commands 'zos-clone help gdg commands' commands "$@"
}
(( $+functions[_zos-clone__help__gdg__create_commands] )) ||
_zos-clone__help__gdg__create_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help gdg create commands' commands "$@"
}
(( $+functions[_zos-clone__help__gdg__delete_commands] )) ||
_zos-clone__help__gdg__delete_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help gdg delete commands' commands "$@"
}
(( $+functions[_zos-clone__help__gdg__list_commands] )) ||
_zos-clone__help__gdg__list_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help gdg list commands' commands "$@"
}
(( $+functions[_zos-clone__help__gdg__new-gen_commands] )) ||
_zos-clone__help__gdg__new-gen_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help gdg new-gen commands' commands "$@"
}
(( $+functions[_zos-clone__help__help_commands] )) ||
_zos-clone__help__help_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help help commands' commands "$@"
}
(( $+functions[_zos-clone__help__idcams_commands] )) ||
_zos-clone__help__idcams_commands() {
    local commands; commands=(
'run:Execute IDCAMS commands from stdin' \
'exec:Execute a single IDCAMS command' \
'file:Execute IDCAMS commands from a file' \
    )
    _describe -t commands 'zos-clone help idcams commands' commands "$@"
}
(( $+functions[_zos-clone__help__idcams__exec_commands] )) ||
_zos-clone__help__idcams__exec_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help idcams exec commands' commands "$@"
}
(( $+functions[_zos-clone__help__idcams__file_commands] )) ||
_zos-clone__help__idcams__file_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help idcams file commands' commands "$@"
}
(( $+functions[_zos-clone__help__idcams__run_commands] )) ||
_zos-clone__help__idcams__run_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help idcams run commands' commands "$@"
}
(( $+functions[_zos-clone__help__interpret_commands] )) ||
_zos-clone__help__interpret_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help interpret commands' commands "$@"
}
(( $+functions[_zos-clone__help__lex_commands] )) ||
_zos-clone__help__lex_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help lex commands' commands "$@"
}
(( $+functions[_zos-clone__help__parse-jcl_commands] )) ||
_zos-clone__help__parse-jcl_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help parse-jcl commands' commands "$@"
}
(( $+functions[_zos-clone__help__run_commands] )) ||
_zos-clone__help__run_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone help run commands' commands "$@"
}
(( $+functions[_zos-clone__idcams_commands] )) ||
_zos-clone__idcams_commands() {
    local commands; commands=(
'run:Execute IDCAMS commands from stdin' \
'exec:Execute a single IDCAMS command' \
'file:Execute IDCAMS commands from a file' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'zos-clone idcams commands' commands "$@"
}
(( $+functions[_zos-clone__idcams__exec_commands] )) ||
_zos-clone__idcams__exec_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone idcams exec commands' commands "$@"
}
(( $+functions[_zos-clone__idcams__file_commands] )) ||
_zos-clone__idcams__file_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone idcams file commands' commands "$@"
}
(( $+functions[_zos-clone__idcams__help_commands] )) ||
_zos-clone__idcams__help_commands() {
    local commands; commands=(
'run:Execute IDCAMS commands from stdin' \
'exec:Execute a single IDCAMS command' \
'file:Execute IDCAMS commands from a file' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'zos-clone idcams help commands' commands "$@"
}
(( $+functions[_zos-clone__idcams__help__exec_commands] )) ||
_zos-clone__idcams__help__exec_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone idcams help exec commands' commands "$@"
}
(( $+functions[_zos-clone__idcams__help__file_commands] )) ||
_zos-clone__idcams__help__file_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone idcams help file commands' commands "$@"
}
(( $+functions[_zos-clone__idcams__help__help_commands] )) ||
_zos-clone__idcams__help__help_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone idcams help help commands' commands "$@"
}
(( $+functions[_zos-clone__idcams__help__run_commands] )) ||
_zos-clone__idcams__help__run_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone idcams help run commands' commands "$@"
}
(( $+functions[_zos-clone__idcams__run_commands] )) ||
_zos-clone__idcams__run_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone idcams run commands' commands "$@"
}
(( $+functions[_zos-clone__interpret_commands] )) ||
_zos-clone__interpret_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone interpret commands' commands "$@"
}
(( $+functions[_zos-clone__lex_commands] )) ||
_zos-clone__lex_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone lex commands' commands "$@"
}
(( $+functions[_zos-clone__parse-jcl_commands] )) ||
_zos-clone__parse-jcl_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone parse-jcl commands' commands "$@"
}
(( $+functions[_zos-clone__run_commands] )) ||
_zos-clone__run_commands() {
    local commands; commands=()
    _describe -t commands 'zos-clone run commands' commands "$@"
}

if [ "$funcstack[1]" = "_zos-clone" ]; then
    _zos-clone "$@"
else
    compdef _zos-clone zos-clone
fi
