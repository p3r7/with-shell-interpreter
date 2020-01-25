# with-shell-interpreter

Helper for Emacs shell command APIs.

This package is a library and does not provide any command.

For more context, read the [accompanying blog post](https://www.eigenbahn.com/2020/01/19/painless-emacs-shell-commands).


## Installation

Not yet on [Melpa](https://melpa.org/).

For now, the recommended way to install is with [use-package](https://github.com/jwiegley/use-package), [quelpa](https://github.com/quelpa/quelpa) and [quelpa-use-package](https://github.com/quelpa/quelpa-use-package).

```el
(use-package with-shell-interpreter
  :quelpa (with-shell-interpreter :fetcher github :repo "p3r7/with-shell-interpreter"))
```

## Configuration

The package defines 3 variables for configuring the default interpreter for remote connections:

 - `default-remote-shell-interpreter`: takes precedence over `shell-file-name`. Default value is `"/bin/bash"`.
 - `default-remote-shell-interpreter-args`: takes precedence over `explicit-INTEPRETER-args`. Default value is `'("-c" "export EMACS=; export TERM=dumb; stty echo; bash")`.
 - `default-remote-shell-interpreter-command-swith`: takes precedence over `shell-command-switch`. Default value is `-c`.

We want this behavior as the user might have redefined the value of `shell-file-name` with something exotic (e.g. zsh) and we would want a safer default for remote servers.

Furthermore, under Microsoft Windows, `shell-file-name` defaults to _cmdproxy.exe_ which is OK for local shells but sucks for remote ones...

These values can be overriden with keyword arguments _:interpreter_, _::interpreter-args_ and _:command-switch_ respectively.


## Usage

We recommand using the macro `with-shell-interpreter`. It's a more convenient version of `eval-with-shell-interpreter` that prevents having to quote _:form_ and wrap it in a `progn`.

| keyword argument  | implicit var being let-bound                   | mandatory?         | description                                                        |
|-------------------|------------------------------------------------|--------------------|--------------------------------------------------------------------|
| :form             |                                                | :heavy_check_mark: | The s-expressions to eval.                                         |
| :path             | `default-directory`                            | :x:                | The path from which to eval. Use a remote path for remote servers. |
| :interpreter      | `explicit-shell-file-name` / `shell-file-name` | :x:                | Name or absolute path of shell interpreter executable.             |
| :interpreter-args | `explicit-INTEPRETER-args`                     | :x:                | Login args to call interpreter with for login.                     |
| :command-switch   | `shell-command-switch`                         | :x:                | Command switch arg for asking interpreter to run a shell command.  |

_:interpreter-args_ is only usefull for interactive shells (from package shell-mode).

_:command-switch_ is only usefull for single shell commands (from package simple).


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


## Legibility

This code uses form feeds (`^L` character) as separators.

Package [form-feed](https://github.com/wasamasa/form-feed) makes them appear as intended.

Package [lisp-extra-font-lock](https://github.com/Lindydancer/lisp-extra-font-lock) is also recommanded to distinguish between local and global vars in _let_ expressions.
