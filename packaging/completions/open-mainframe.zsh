#compdef open-mainframe

autoload -U is-at-least

_open-mainframe() {
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
":: :_open-mainframe_commands" \
"*::: :->open-mainframe" \
&& ret=0
    case $state in
    (open-mainframe)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:open-mainframe-command-$line[1]:"
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
":: :_open-mainframe__gdg_commands" \
"*::: :->gdg" \
&& ret=0

    case $state in
    (gdg)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:open-mainframe-gdg-command-$line[1]:"
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
":: :_open-mainframe__gdg__help_commands" \
"*::: :->help" \
&& ret=0

    case $state in
    (help)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:open-mainframe-gdg-help-command-$line[1]:"
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
":: :_open-mainframe__idcams_commands" \
"*::: :->idcams" \
&& ret=0

    case $state in
    (idcams)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:open-mainframe-idcams-command-$line[1]:"
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
":: :_open-mainframe__idcams__help_commands" \
"*::: :->help" \
&& ret=0

    case $state in
    (help)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:open-mainframe-idcams-help-command-$line[1]:"
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
":: :_open-mainframe__config_commands" \
"*::: :->config" \
&& ret=0

    case $state in
    (config)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:open-mainframe-config-command-$line[1]:"
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
":: :_open-mainframe__config__help_commands" \
"*::: :->help" \
&& ret=0

    case $state in
    (help)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:open-mainframe-config-help-command-$line[1]:"
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
":: :_open-mainframe__help_commands" \
"*::: :->help" \
&& ret=0

    case $state in
    (help)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:open-mainframe-help-command-$line[1]:"
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
":: :_open-mainframe__help__gdg_commands" \
"*::: :->gdg" \
&& ret=0

    case $state in
    (gdg)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:open-mainframe-help-gdg-command-$line[1]:"
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
":: :_open-mainframe__help__idcams_commands" \
"*::: :->idcams" \
&& ret=0

    case $state in
    (idcams)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:open-mainframe-help-idcams-command-$line[1]:"
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
":: :_open-mainframe__help__config_commands" \
"*::: :->config" \
&& ret=0

    case $state in
    (config)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:open-mainframe-help-config-command-$line[1]:"
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

(( $+functions[_open-mainframe_commands] )) ||
_open-mainframe_commands() {
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
    _describe -t commands 'open-mainframe commands' commands "$@"
}
(( $+functions[_open-mainframe__check_commands] )) ||
_open-mainframe__check_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe check commands' commands "$@"
}
(( $+functions[_open-mainframe__compile_commands] )) ||
_open-mainframe__compile_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe compile commands' commands "$@"
}
(( $+functions[_open-mainframe__completions_commands] )) ||
_open-mainframe__completions_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe completions commands' commands "$@"
}
(( $+functions[_open-mainframe__config_commands] )) ||
_open-mainframe__config_commands() {
    local commands; commands=(
'show:Show current configuration' \
'init:Generate a default configuration file' \
'paths:Show configuration file paths' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'open-mainframe config commands' commands "$@"
}
(( $+functions[_open-mainframe__config__help_commands] )) ||
_open-mainframe__config__help_commands() {
    local commands; commands=(
'show:Show current configuration' \
'init:Generate a default configuration file' \
'paths:Show configuration file paths' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'open-mainframe config help commands' commands "$@"
}
(( $+functions[_open-mainframe__config__help__help_commands] )) ||
_open-mainframe__config__help__help_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe config help help commands' commands "$@"
}
(( $+functions[_open-mainframe__config__help__init_commands] )) ||
_open-mainframe__config__help__init_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe config help init commands' commands "$@"
}
(( $+functions[_open-mainframe__config__help__paths_commands] )) ||
_open-mainframe__config__help__paths_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe config help paths commands' commands "$@"
}
(( $+functions[_open-mainframe__config__help__show_commands] )) ||
_open-mainframe__config__help__show_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe config help show commands' commands "$@"
}
(( $+functions[_open-mainframe__config__init_commands] )) ||
_open-mainframe__config__init_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe config init commands' commands "$@"
}
(( $+functions[_open-mainframe__config__paths_commands] )) ||
_open-mainframe__config__paths_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe config paths commands' commands "$@"
}
(( $+functions[_open-mainframe__config__show_commands] )) ||
_open-mainframe__config__show_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe config show commands' commands "$@"
}
(( $+functions[_open-mainframe__gdg_commands] )) ||
_open-mainframe__gdg_commands() {
    local commands; commands=(
'create:Create a new GDG base' \
'list:List GDG generations' \
'delete:Delete a GDG base and all generations' \
'new-gen:Create a new generation' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'open-mainframe gdg commands' commands "$@"
}
(( $+functions[_open-mainframe__gdg__create_commands] )) ||
_open-mainframe__gdg__create_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe gdg create commands' commands "$@"
}
(( $+functions[_open-mainframe__gdg__delete_commands] )) ||
_open-mainframe__gdg__delete_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe gdg delete commands' commands "$@"
}
(( $+functions[_open-mainframe__gdg__help_commands] )) ||
_open-mainframe__gdg__help_commands() {
    local commands; commands=(
'create:Create a new GDG base' \
'list:List GDG generations' \
'delete:Delete a GDG base and all generations' \
'new-gen:Create a new generation' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'open-mainframe gdg help commands' commands "$@"
}
(( $+functions[_open-mainframe__gdg__help__create_commands] )) ||
_open-mainframe__gdg__help__create_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe gdg help create commands' commands "$@"
}
(( $+functions[_open-mainframe__gdg__help__delete_commands] )) ||
_open-mainframe__gdg__help__delete_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe gdg help delete commands' commands "$@"
}
(( $+functions[_open-mainframe__gdg__help__help_commands] )) ||
_open-mainframe__gdg__help__help_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe gdg help help commands' commands "$@"
}
(( $+functions[_open-mainframe__gdg__help__list_commands] )) ||
_open-mainframe__gdg__help__list_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe gdg help list commands' commands "$@"
}
(( $+functions[_open-mainframe__gdg__help__new-gen_commands] )) ||
_open-mainframe__gdg__help__new-gen_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe gdg help new-gen commands' commands "$@"
}
(( $+functions[_open-mainframe__gdg__list_commands] )) ||
_open-mainframe__gdg__list_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe gdg list commands' commands "$@"
}
(( $+functions[_open-mainframe__gdg__new-gen_commands] )) ||
_open-mainframe__gdg__new-gen_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe gdg new-gen commands' commands "$@"
}
(( $+functions[_open-mainframe__help_commands] )) ||
_open-mainframe__help_commands() {
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
    _describe -t commands 'open-mainframe help commands' commands "$@"
}
(( $+functions[_open-mainframe__help__check_commands] )) ||
_open-mainframe__help__check_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help check commands' commands "$@"
}
(( $+functions[_open-mainframe__help__compile_commands] )) ||
_open-mainframe__help__compile_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help compile commands' commands "$@"
}
(( $+functions[_open-mainframe__help__completions_commands] )) ||
_open-mainframe__help__completions_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help completions commands' commands "$@"
}
(( $+functions[_open-mainframe__help__config_commands] )) ||
_open-mainframe__help__config_commands() {
    local commands; commands=(
'show:Show current configuration' \
'init:Generate a default configuration file' \
'paths:Show configuration file paths' \
    )
    _describe -t commands 'open-mainframe help config commands' commands "$@"
}
(( $+functions[_open-mainframe__help__config__init_commands] )) ||
_open-mainframe__help__config__init_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help config init commands' commands "$@"
}
(( $+functions[_open-mainframe__help__config__paths_commands] )) ||
_open-mainframe__help__config__paths_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help config paths commands' commands "$@"
}
(( $+functions[_open-mainframe__help__config__show_commands] )) ||
_open-mainframe__help__config__show_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help config show commands' commands "$@"
}
(( $+functions[_open-mainframe__help__gdg_commands] )) ||
_open-mainframe__help__gdg_commands() {
    local commands; commands=(
'create:Create a new GDG base' \
'list:List GDG generations' \
'delete:Delete a GDG base and all generations' \
'new-gen:Create a new generation' \
    )
    _describe -t commands 'open-mainframe help gdg commands' commands "$@"
}
(( $+functions[_open-mainframe__help__gdg__create_commands] )) ||
_open-mainframe__help__gdg__create_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help gdg create commands' commands "$@"
}
(( $+functions[_open-mainframe__help__gdg__delete_commands] )) ||
_open-mainframe__help__gdg__delete_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help gdg delete commands' commands "$@"
}
(( $+functions[_open-mainframe__help__gdg__list_commands] )) ||
_open-mainframe__help__gdg__list_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help gdg list commands' commands "$@"
}
(( $+functions[_open-mainframe__help__gdg__new-gen_commands] )) ||
_open-mainframe__help__gdg__new-gen_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help gdg new-gen commands' commands "$@"
}
(( $+functions[_open-mainframe__help__help_commands] )) ||
_open-mainframe__help__help_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help help commands' commands "$@"
}
(( $+functions[_open-mainframe__help__idcams_commands] )) ||
_open-mainframe__help__idcams_commands() {
    local commands; commands=(
'run:Execute IDCAMS commands from stdin' \
'exec:Execute a single IDCAMS command' \
'file:Execute IDCAMS commands from a file' \
    )
    _describe -t commands 'open-mainframe help idcams commands' commands "$@"
}
(( $+functions[_open-mainframe__help__idcams__exec_commands] )) ||
_open-mainframe__help__idcams__exec_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help idcams exec commands' commands "$@"
}
(( $+functions[_open-mainframe__help__idcams__file_commands] )) ||
_open-mainframe__help__idcams__file_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help idcams file commands' commands "$@"
}
(( $+functions[_open-mainframe__help__idcams__run_commands] )) ||
_open-mainframe__help__idcams__run_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help idcams run commands' commands "$@"
}
(( $+functions[_open-mainframe__help__interpret_commands] )) ||
_open-mainframe__help__interpret_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help interpret commands' commands "$@"
}
(( $+functions[_open-mainframe__help__lex_commands] )) ||
_open-mainframe__help__lex_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help lex commands' commands "$@"
}
(( $+functions[_open-mainframe__help__parse-jcl_commands] )) ||
_open-mainframe__help__parse-jcl_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help parse-jcl commands' commands "$@"
}
(( $+functions[_open-mainframe__help__run_commands] )) ||
_open-mainframe__help__run_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe help run commands' commands "$@"
}
(( $+functions[_open-mainframe__idcams_commands] )) ||
_open-mainframe__idcams_commands() {
    local commands; commands=(
'run:Execute IDCAMS commands from stdin' \
'exec:Execute a single IDCAMS command' \
'file:Execute IDCAMS commands from a file' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'open-mainframe idcams commands' commands "$@"
}
(( $+functions[_open-mainframe__idcams__exec_commands] )) ||
_open-mainframe__idcams__exec_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe idcams exec commands' commands "$@"
}
(( $+functions[_open-mainframe__idcams__file_commands] )) ||
_open-mainframe__idcams__file_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe idcams file commands' commands "$@"
}
(( $+functions[_open-mainframe__idcams__help_commands] )) ||
_open-mainframe__idcams__help_commands() {
    local commands; commands=(
'run:Execute IDCAMS commands from stdin' \
'exec:Execute a single IDCAMS command' \
'file:Execute IDCAMS commands from a file' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'open-mainframe idcams help commands' commands "$@"
}
(( $+functions[_open-mainframe__idcams__help__exec_commands] )) ||
_open-mainframe__idcams__help__exec_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe idcams help exec commands' commands "$@"
}
(( $+functions[_open-mainframe__idcams__help__file_commands] )) ||
_open-mainframe__idcams__help__file_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe idcams help file commands' commands "$@"
}
(( $+functions[_open-mainframe__idcams__help__help_commands] )) ||
_open-mainframe__idcams__help__help_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe idcams help help commands' commands "$@"
}
(( $+functions[_open-mainframe__idcams__help__run_commands] )) ||
_open-mainframe__idcams__help__run_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe idcams help run commands' commands "$@"
}
(( $+functions[_open-mainframe__idcams__run_commands] )) ||
_open-mainframe__idcams__run_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe idcams run commands' commands "$@"
}
(( $+functions[_open-mainframe__interpret_commands] )) ||
_open-mainframe__interpret_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe interpret commands' commands "$@"
}
(( $+functions[_open-mainframe__lex_commands] )) ||
_open-mainframe__lex_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe lex commands' commands "$@"
}
(( $+functions[_open-mainframe__parse-jcl_commands] )) ||
_open-mainframe__parse-jcl_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe parse-jcl commands' commands "$@"
}
(( $+functions[_open-mainframe__run_commands] )) ||
_open-mainframe__run_commands() {
    local commands; commands=()
    _describe -t commands 'open-mainframe run commands' commands "$@"
}

if [ "$funcstack[1]" = "_open-mainframe" ]; then
    _open-mainframe "$@"
else
    compdef _open-mainframe open-mainframe
fi
