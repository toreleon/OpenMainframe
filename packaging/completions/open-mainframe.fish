# Print an optspec for argparse to handle cmd's options that are independent of any subcommand.
function __fish_open_mainframe_global_optspecs
	string join \n v/verbose h/help V/version
end

function __fish_open_mainframe_needs_command
	# Figure out if the current invocation already has a command.
	set -l cmd (commandline -opc)
	set -e cmd[1]
	argparse -s (__fish_open_mainframe_global_optspecs) -- $cmd 2>/dev/null
	or return
	if set -q argv[1]
		# Also print the command, so this can be used to figure out what it is.
		echo $argv[1]
		return 1
	end
	return 0
end

function __fish_open_mainframe_using_subcommand
	set -l cmd (__fish_open_mainframe_needs_command)
	test -z "$cmd"
	and return 1
	contains -- $cmd[1] $argv
end

complete -c open-mainframe -n "__fish_open_mainframe_needs_command" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_needs_command" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_needs_command" -s V -l version -d 'Print version'
complete -c open-mainframe -n "__fish_open_mainframe_needs_command" -f -a "compile" -d 'Compile a COBOL source file'
complete -c open-mainframe -n "__fish_open_mainframe_needs_command" -f -a "run" -d 'Run a JCL job'
complete -c open-mainframe -n "__fish_open_mainframe_needs_command" -f -a "check" -d 'Check COBOL syntax without compiling'
complete -c open-mainframe -n "__fish_open_mainframe_needs_command" -f -a "parse-jcl" -d 'Parse JCL and show the job structure'
complete -c open-mainframe -n "__fish_open_mainframe_needs_command" -f -a "lex" -d 'Show COBOL tokens (for debugging)'
complete -c open-mainframe -n "__fish_open_mainframe_needs_command" -f -a "interpret" -d 'Interpret a COBOL program (run without compiling)'
complete -c open-mainframe -n "__fish_open_mainframe_needs_command" -f -a "gdg" -d 'Manage Generation Data Groups (GDG)'
complete -c open-mainframe -n "__fish_open_mainframe_needs_command" -f -a "idcams" -d 'Run IDCAMS commands for dataset management'
complete -c open-mainframe -n "__fish_open_mainframe_needs_command" -f -a "config" -d 'Manage configuration'
complete -c open-mainframe -n "__fish_open_mainframe_needs_command" -f -a "completions" -d 'Generate shell completions'
complete -c open-mainframe -n "__fish_open_mainframe_needs_command" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand compile" -s o -l output -d 'Output file path' -r -F
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand compile" -s O -l optimize -d 'Optimization level (0-3)' -r
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand compile" -s I -l include -d 'Additional copybook search paths' -r -F
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand compile" -l emit-asm -d 'Emit assembly instead of object file'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand compile" -l emit-llvm -d 'Emit LLVM IR instead of object file'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand compile" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand compile" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand run" -l program-dir -d 'Directory containing compiled programs' -r -F
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand run" -l dataset-dir -d 'Directory for dataset files' -r -F
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand run" -l work-dir -d 'Working directory for job execution' -r -F
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand run" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand run" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand check" -s I -l include -d 'Additional copybook search paths' -r -F
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand check" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand check" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand parse-jcl" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand parse-jcl" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand lex" -l format -d 'Source format (fixed, free, or auto)' -r
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand lex" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand lex" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand interpret" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand interpret" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and not __fish_seen_subcommand_from create list delete new-gen help" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and not __fish_seen_subcommand_from create list delete new-gen help" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and not __fish_seen_subcommand_from create list delete new-gen help" -f -a "create" -d 'Create a new GDG base'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and not __fish_seen_subcommand_from create list delete new-gen help" -f -a "list" -d 'List GDG generations'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and not __fish_seen_subcommand_from create list delete new-gen help" -f -a "delete" -d 'Delete a GDG base and all generations'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and not __fish_seen_subcommand_from create list delete new-gen help" -f -a "new-gen" -d 'Create a new generation'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and not __fish_seen_subcommand_from create list delete new-gen help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from create" -s l -l limit -d 'Maximum number of generations to keep (1-255)' -r
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from create" -l dataset-dir -d 'Base directory for datasets' -r -F
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from create" -l scratch -d 'Delete old generations when rolling off (default: true)'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from create" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from create" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from list" -l dataset-dir -d 'Base directory for datasets' -r -F
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from list" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from list" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from delete" -l dataset-dir -d 'Base directory for datasets' -r -F
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from delete" -s f -l force -d 'Force deletion even if generations exist'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from delete" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from delete" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from new-gen" -l dataset-dir -d 'Base directory for datasets' -r -F
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from new-gen" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from new-gen" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from help" -f -a "create" -d 'Create a new GDG base'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from help" -f -a "list" -d 'List GDG generations'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from help" -f -a "delete" -d 'Delete a GDG base and all generations'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from help" -f -a "new-gen" -d 'Create a new generation'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand gdg; and __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and not __fish_seen_subcommand_from run exec file help" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and not __fish_seen_subcommand_from run exec file help" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and not __fish_seen_subcommand_from run exec file help" -f -a "run" -d 'Execute IDCAMS commands from stdin'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and not __fish_seen_subcommand_from run exec file help" -f -a "exec" -d 'Execute a single IDCAMS command'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and not __fish_seen_subcommand_from run exec file help" -f -a "file" -d 'Execute IDCAMS commands from a file'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and not __fish_seen_subcommand_from run exec file help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and __fish_seen_subcommand_from run" -l dataset-dir -d 'Base directory for datasets' -r -F
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and __fish_seen_subcommand_from run" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and __fish_seen_subcommand_from run" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and __fish_seen_subcommand_from exec" -l dataset-dir -d 'Base directory for datasets' -r -F
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and __fish_seen_subcommand_from exec" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and __fish_seen_subcommand_from exec" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and __fish_seen_subcommand_from file" -l dataset-dir -d 'Base directory for datasets' -r -F
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and __fish_seen_subcommand_from file" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and __fish_seen_subcommand_from file" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and __fish_seen_subcommand_from help" -f -a "run" -d 'Execute IDCAMS commands from stdin'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and __fish_seen_subcommand_from help" -f -a "exec" -d 'Execute a single IDCAMS command'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and __fish_seen_subcommand_from help" -f -a "file" -d 'Execute IDCAMS commands from a file'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand idcams; and __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and not __fish_seen_subcommand_from show init paths help" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and not __fish_seen_subcommand_from show init paths help" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and not __fish_seen_subcommand_from show init paths help" -f -a "show" -d 'Show current configuration'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and not __fish_seen_subcommand_from show init paths help" -f -a "init" -d 'Generate a default configuration file'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and not __fish_seen_subcommand_from show init paths help" -f -a "paths" -d 'Show configuration file paths'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and not __fish_seen_subcommand_from show init paths help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and __fish_seen_subcommand_from show" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and __fish_seen_subcommand_from show" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and __fish_seen_subcommand_from init" -s o -l output -d 'Output path for config file' -r -F
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and __fish_seen_subcommand_from init" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and __fish_seen_subcommand_from init" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and __fish_seen_subcommand_from paths" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and __fish_seen_subcommand_from paths" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and __fish_seen_subcommand_from help" -f -a "show" -d 'Show current configuration'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and __fish_seen_subcommand_from help" -f -a "init" -d 'Generate a default configuration file'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and __fish_seen_subcommand_from help" -f -a "paths" -d 'Show configuration file paths'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand config; and __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand completions" -s v -l verbose -d 'Enable verbose output'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand completions" -s h -l help -d 'Print help'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and not __fish_seen_subcommand_from compile run check parse-jcl lex interpret gdg idcams config completions help" -f -a "compile" -d 'Compile a COBOL source file'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and not __fish_seen_subcommand_from compile run check parse-jcl lex interpret gdg idcams config completions help" -f -a "run" -d 'Run a JCL job'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and not __fish_seen_subcommand_from compile run check parse-jcl lex interpret gdg idcams config completions help" -f -a "check" -d 'Check COBOL syntax without compiling'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and not __fish_seen_subcommand_from compile run check parse-jcl lex interpret gdg idcams config completions help" -f -a "parse-jcl" -d 'Parse JCL and show the job structure'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and not __fish_seen_subcommand_from compile run check parse-jcl lex interpret gdg idcams config completions help" -f -a "lex" -d 'Show COBOL tokens (for debugging)'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and not __fish_seen_subcommand_from compile run check parse-jcl lex interpret gdg idcams config completions help" -f -a "interpret" -d 'Interpret a COBOL program (run without compiling)'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and not __fish_seen_subcommand_from compile run check parse-jcl lex interpret gdg idcams config completions help" -f -a "gdg" -d 'Manage Generation Data Groups (GDG)'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and not __fish_seen_subcommand_from compile run check parse-jcl lex interpret gdg idcams config completions help" -f -a "idcams" -d 'Run IDCAMS commands for dataset management'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and not __fish_seen_subcommand_from compile run check parse-jcl lex interpret gdg idcams config completions help" -f -a "config" -d 'Manage configuration'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and not __fish_seen_subcommand_from compile run check parse-jcl lex interpret gdg idcams config completions help" -f -a "completions" -d 'Generate shell completions'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and not __fish_seen_subcommand_from compile run check parse-jcl lex interpret gdg idcams config completions help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and __fish_seen_subcommand_from gdg" -f -a "create" -d 'Create a new GDG base'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and __fish_seen_subcommand_from gdg" -f -a "list" -d 'List GDG generations'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and __fish_seen_subcommand_from gdg" -f -a "delete" -d 'Delete a GDG base and all generations'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and __fish_seen_subcommand_from gdg" -f -a "new-gen" -d 'Create a new generation'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and __fish_seen_subcommand_from idcams" -f -a "run" -d 'Execute IDCAMS commands from stdin'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and __fish_seen_subcommand_from idcams" -f -a "exec" -d 'Execute a single IDCAMS command'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and __fish_seen_subcommand_from idcams" -f -a "file" -d 'Execute IDCAMS commands from a file'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and __fish_seen_subcommand_from config" -f -a "show" -d 'Show current configuration'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and __fish_seen_subcommand_from config" -f -a "init" -d 'Generate a default configuration file'
complete -c open-mainframe -n "__fish_open_mainframe_using_subcommand help; and __fish_seen_subcommand_from config" -f -a "paths" -d 'Show configuration file paths'
