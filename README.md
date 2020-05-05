# with-shell-interpreter [![MELPA](https://melpa.org/packages/with-shell-interpreter-badge.svg)](https://melpa.org/#/with-shell-interpreter) [![MELPA Stable](https://stable.melpa.org/packages/with-shell-interpreter-badge.svg)](https://stable.melpa.org/#/with-shell-interpreter)


Helper for Emacs shell command APIs, making implicit argument as explicit keyword arguments.

This package is a library and does not provide any command.

It is inspired by the `eval-after-load` / `with-eval-after-load` functions.

It does the heavy lifting behind packages [friendly-shell-command](https://github.com/p3r7/friendly-shell) and [friendly-shell](https://github.com/p3r7/friendly-shell).

For more context, read the [accompanying blog post](https://www.eigenbahn.com/2020/01/19/painless-emacs-shell-commands).


## Installation

The package is available on [Melpa](https://melpa.org/).

With `use-package`:

```el
(use-package with-shell-interpreter)
```

Manually:

    M-x package-install with-shell-interpreter


## Usage

We recommend using the macro `with-shell-interpreter`. It's a more convenient version of `with-shell-interpreter-eval` that prevents having to quote _:form_ and wrap it in a `progn`.

| keyword argument    | implicit var being let-bound                   | mandatory?         | description                                                                       |
|---------------------|------------------------------------------------|--------------------|-----------------------------------------------------------------------------------|
| _:form_             | n/a                                            | :heavy_check_mark: | The s-expressions to eval.                                                        |
| _:path_             | `default-directory`                            | :x:                | The path from which to eval.                                                      |
| _:interpreter_      | `explicit-shell-file-name` / `shell-file-name` | :x:                | Name or absolute path of shell interpreter executable.                            |
| _:interpreter-args_ | `explicit-INTEPRETER-args`                     | :x:                | Login args to call interpreter with for login.                                    |
| _:command-switch_   | `shell-command-switch`                         | :x:                | Command switch arg for asking interpreter to run a shell command.                 |
| _:w32-arg-quote_    | `w32-quote-process-args`                       | :x:                | Character to use for quoting shell arguments (only on the Windows build of Emacs) |
| _:allow-local-vars_ | n/a                                            | :x:                | Whether to allow buffer-local and/or connection-local values                      |

_:form_ is expected to contain calls to functions relying on the Emacs shell APIs (e.g. `shell`, `shell-command`, `async-shell-command` and `shell-command-to-string`).

Setting _:path_ to a remote location (with TRAMP format, i.e. `/<method>:<user>@<host>:<localname>`) allows running form with interpreter of remote server.

_:interpreter-args_ is only usefull for interactive shells (from package shell-mode).

_:command-switch_ is only usefull for single shell commands (from package simple).

_:allow-local-vars_ can take the following values:

 - _'buffer_: allow buffer-local vars values_
 - _'connection_: allow connection-local values
 - _'both_: allow both types of local values
 - _'none_: ignore all local values

If left empty, here are the default values being used:

| keyword argument    | fallback value (local path)       | fallback value (remote path)                           |
|---------------------|-----------------------------------|--------------------------------------------------------|
| _:path_             | current `default-directory`       | current `default-directory`                            |
| _:interpreter_      | `shell-file-name`                 | `with-shell-interpreter-default-remote`                |
| _:interpreter-args_ | `explicit-INTEPRETER-args` if set | `with-shell-interpreter-default-remote-args`           |
| _:command-switch_   | `shell-command-switch`            | `with-shell-interpreter-default-remote-command-switch` |
| _:w32-arg-quote_    | `w32-quote-process-args`          | `w32-quote-process-args`                               |
| _:allow-local-vars_ | _nil_                             | _'connection_                                          |


#### Example

Getting the temperature from a Raspberry Pi:

```el
(with-shell-interpreter
   :path "/ssh:pi@raspberry:/~"
   :interpreter "bash"
   :form
   (shell-command-to-string "vcgencmd measure_temp"))
```

Under Microsoft Windows, launching an interactive shell with the git-bash interpreter:

```el
(with-shell-interpreter
   :path "~"                            ; ensure local path
   :interpreter "C:/Program Files/Git/bin/bash.exe"
   :form
   (let (current-prefix-arg '(4))       ; don't prompt user for interpreter
        (shell)))
```

For more practical examples, have a look at packages in [friendly-shell](https://github.com/p3r7/friendly-shell) ([examples](https://github.com/p3r7/friendly-shell/blob/master/examples.md)).


## Configuration

The package defines 3 variables for configuring the default interpreter for remote connections:

 - `with-shell-interpreter-default-remote`: takes precedence over `shell-file-name`. Default value is `"/bin/bash"`.
 - `with-shell-interpreter-default-remote-args`: takes precedence over `explicit-INTEPRETER-args`. Default value is `'("-c" "export EMACS=; export TERM=dumb; stty echo; bash")`.
 - `with-shell-interpreter-default-remote-command-switch`: takes precedence over `shell-command-switch`. Default value is `-c`.

We want this behavior as the user might have redefined the value of `shell-file-name` with something exotic (e.g. zsh) and we would want a safer default for remote servers.

Furthermore, under Microsoft Windows, `shell-file-name` defaults to _cmdproxy.exe_ which is OK for local shells but sucks for remote ones...

These values can be overridden with keyword arguments _:interpreter_, _:interpreter-args_ and _:command-switch_ respectively.

Additionally, you might want to change the value of `tramp-default-user` if you usually connect to remote host with a user different than your local one.


## Legibility

This code uses form feeds (`^L` character) as separators.

Package [form-feed](https://github.com/wasamasa/form-feed) makes them appear as intended.

Package [lisp-extra-font-lock](https://github.com/Lindydancer/lisp-extra-font-lock) is also recommended to distinguish between local and global vars in _let_ expressions.
