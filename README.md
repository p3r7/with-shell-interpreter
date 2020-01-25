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

## Usage

```el
(with-shell-interpreter
   :path "/ssh:pi@raspberry:/~"
   :interpreter "bash"
   :form
   (shell-command-to-string "vcgencmd measure_temp"))
```


## Legibility

This code uses form feeds (`^L` character) as separators.

Package [form-feed](https://github.com/wasamasa/form-feed) makes them appear as intended.

Package [lisp-extra-font-lock](https://github.com/Lindydancer/lisp-extra-font-lock) is also recommanded to distinguish between local and global vars in _let_ expressions.
