;;; with-shell-interpreter.el --- Helper for shell command APIs -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.2.4
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/with-shell-interpreter
;; Package-Requires: ((emacs "25.1")(cl-lib "0.6.1"))
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Helper macro for Emacs shell command APIs, making implicit argument as explicit keyword arguments.
;; Provides macro `with-shell-interpreter'.
;;
;; For detailed instructions, please look at the README.md at https://github.com/p3r7/with-shell-interpreter/blob/master/README.md

;;; Code:



;; REQUIRES

(require 'cl-lib)

(require 'files-x)
(require 'shell)



;; VARS

(defvar with-shell-interpreter-connection-local-vars-implem
  'custom
  "Implementation of connection-local vars to use.
Possible values:
 - 'custom: with-shell-interpreter's implementation
 - 'native: native Emacs (>= 26.1) implementation

When 'custom, configure var
`with-shell-interpreter-connection-local-vars'.

When 'native, configure vars `connection-local-profile-alist' and
`connection-local-criteria-alist' with helper functions
`connection-local-set-profile-variables' and
`connection-local-set-profiles'.")

(defvar with-shell-interpreter-connection-local-vars
  '((".*" . ((explicit-shell-file-name . "/bin/bash")
             (explicit-bash-args . ("-c" "export EMACS=; export TERM=dumb; stty echo; bash"))
             (shell-command-switch . "-c"))))
  "Alist mapping connection path regexp to variable lists.
It aims at providing a more flexible implementation of
connection-local variables.

Order of entries matter, only first matched variables are used.
Use `add-to-list' to add entries.

It only get interpreted for remote connections.
For local connections, customize defautl values of vars:
 - `explicit-shell-file-name' / `shell-file-name'
 - `explicit-INTEPRETER-args'
 - `shell-command-switch'

To read more about the standard connection-local variables see
`with-shell-interpreter-connection-local-vars-implem'.")



;; COMPATIBILITY

;; NB: connection-local variables are only available since version 26.1
(eval-and-compile
  (if (fboundp 'hack-connection-local-variables)
      (defalias 'with-shell-interpreter--hack-connection-local-variables #'hack-connection-local-variables)
    (defalias 'with-shell-interpreter--hack-connection-local-variables (lambda (_c) nil))))

;; NB: only bound on Windows build of Emacs
(unless (boundp 'w32-quote-process-args)
  ;; tame lexical binding warnings
  (defvar w32-quote-process-args nil))



;; MAIN HELPER

(defmacro with-shell-interpreter (&rest args)
  "Eval :form at location described by :path with :interpreter binary.

ARGS are in fact keywords, `with-shell-interpreter' being a macro wrapper around
`with-shell-interpreter-eval'.  Usage:

  (with-shell-interpreter
     [:keyword [option]]...
     :form
     ;; actual code
     )

:form               Code to execute.
:path               Location from which form is executed.
                    Can be local or remote (TRAMP format).
                    Let-binds `default-directory'.
:interpreter        Name or absolute path of shell interpreter executable.
                    If only providing a name, ensure that the executable
                    is present in the PATH.
                    Let-binds `explicit-shell-file-name' and
                    `shell-file-name'.
:interpreter-args   Login args to call interpreter with for login.
                    Let-binds `explicit-INTEPRETER-args'.
                    Useful only for interactive shells.
:command-switch     Command switch arg for asking interpreter to run a
                    shell command (e.g. \"-c\" in Bourne shell and most
                    derivatives).
                    Let-binds `shell-command-switch'.
                    Useful only for single shell commands.
:w32-arg-quote      Only affecting Microsoft Windows build of Emacs.
                    Character to use for quoting arguments.
                    Let-binds `w32-quote-process-args'.
:allow-local-vars   Allow local values to have precedence over global ones
                    for:
                     - `explicit-shell-file-name'
                     - `explicit-INTEPRETER-args'
                     - `shell-command-switch'
                     - `w32-quote-process-args'
                    Value can be:
                      - 'buffer: allow buffer-local vars values
                      - 'connection: allow connection-local values
                      - 'both: allow both types of local values
                      - 'none: ignore all local values
                    Default is 'connection.

For more detailed instructions, have a look at https://github.com/p3r7/with-shell-interpreter/blob/master/README.md"
  (declare (indent 1) (debug t))
  `(with-shell-interpreter-eval
    :form (lambda () ,(cons 'progn (with-shell-interpreter--plist-get args :form)))
    :path ,(plist-get args :path)
    :interpreter ,(plist-get args :interpreter)
    :interpreter-args ,(plist-get args :interpreter-args)
    :command-switch ,(plist-get args :command-switch)
    :w32-arg-quote ,(plist-get args :w32-arg-quote)
    :allow-local-vars ,(plist-get args :allow-local-vars)))

(put 'with-shell-interpreter 'lisp-indent-function 'defun)

(cl-defun with-shell-interpreter-eval (&key form path
                                            interpreter interpreter-args command-switch
                                            w32-arg-quote
                                            allow-local-vars)
  "Same as `with-shell-interpreter' except :form has to be a quoted sexp."
  (cl-destructuring-bind (path
                          is-remote
                          allow-buffer-local-vars
                          allow-cnnx-local-vars cnnx-local-vars
                          interpreter interpreter-name
                          explicit-interpreter-args-var)
      (with-shell-interpreter--generate-props path interpreter allow-local-vars)
    (let* ((func
            (if (functionp form) form
              ;; Try to use the "current" lexical/dynamic mode for `form'.
              (eval `(lambda () ,form) lexical-binding)))
           (interpreter-args (with-shell-interpreter--resolve-shell-var
                              explicit-interpreter-args-var
                              is-remote allow-buffer-local-vars allow-cnnx-local-vars cnnx-local-vars
                              interpreter-args '("-i") t interpreter))
           (command-switch (with-shell-interpreter--resolve-shell-var
                            'shell-command-switch
                            is-remote allow-buffer-local-vars allow-cnnx-local-vars cnnx-local-vars
                            command-switch "-c" t interpreter))
           ;; bellow are vars acting as implicit options to shell functions
           (default-directory path)
           (shell-file-name interpreter)
           (explicit-shell-file-name interpreter)
           (shell-command-switch command-switch)
           (enable-connection-local-variables nil) ; disable lookup of connection-local vars in :form
           ;; NB: w32-only feature
           (w32-quote-process-args (with-shell-interpreter--resolve-shell-var
                                    'w32-quote-process-args
                                    is-remote allow-buffer-local-vars allow-cnnx-local-vars cnnx-local-vars
                                    w32-arg-quote nil t interpreter)))
      (cl-progv
          (list explicit-interpreter-args-var)
          (list interpreter-args)
        (funcall func)))))



;; HELPERS: STRING NORMALIZATION

(defun with-shell-interpreter--normalize-path (path)
  "Normalize PATH, converting \\ into /."
  ;; REVIEW: shouldn't we just use instead `convert-standard-filename'
  ;; or even `executable-find'?
  (subst-char-in-string ?\\ ?/ path))


(defun with-shell-interpreter--interpreter-name (interpreter)
  "Extracts INTERPRETER name, keeping extension."
  (file-name-nondirectory interpreter))



;; HELPERS: STRUCTURES

(defun with-shell-interpreter--plist-get (plist prop)
  "Extract value of property PROP from property list PLIST.
Like `plist-get' except allows value to be multiple elements."
  (when plist
    (cl-loop with passed = nil
             for e in plist
             until (and passed
                        (keywordp e)
                        (not (eq e prop)))
             if (and passed
                     (not (keywordp e)))
             collect e
             else if (and (not passed)
                          (keywordp e)
                          (eq e prop))
             do (setq passed 't))))

(defun with-shell-interpreter--some (fn list)
  "Return (FN x) for the first LIST item where (FN x) is non-nil."
  (let (res)
    (while (not res)
      (setq res (funcall fn (car list))
            list (cdr list)))
    res))



;; HELPERS: VARIABLES SCOPE

(defun with-shell-interpreter--symbol-value (sym &optional allow-buffer-local)
  "Return the value of SYM in current buffer.
If ALLOW-BUFFER-LOCAL is nil, always return global value (never buffer-local one)."
  (if (not allow-buffer-local)
      ;; NB: if local-only `default-value' throws an error
      (ignore-errors
        (default-value sym))
    (symbol-value sym)))


(defun with-shell-interpreter--boundp-buffer-local (symbol)
  "Return t if SYMBOL has a buffer-local value.
Even works if it's value is nil."
  (assoc symbol (buffer-local-variables)))



;; HELPERS: CONNECTION-LOCAL VARS

(defun with-shell-interpreter--cnnx-local-vars (path)
  "Get connection-local vars for PATH."
  (if (eq with-shell-interpreter-connection-local-vars-implem 'custom)
      (with-shell-interpreter--cnnx-local-vars-custom path)
    (with-shell-interpreter--cnnx-local-vars-native path)))

(defun with-shell-interpreter--cnnx-local-vars-custom (path)
  "Get connection-local vars for PATH (custom)."
  (when (file-remote-p path)
    (with-shell-interpreter--some
     (lambda (e)
       (let ((regexp (car e))
             (vars (cdr e)))
         (when (string-match regexp path)
           vars)))
     with-shell-interpreter-connection-local-vars)))

(defun with-shell-interpreter--cnnx-local-vars-native (path)
  "Get connection-local vars for PATH (native)."
  (when (file-remote-p path)
    (let (output)
      (with-temp-buffer
        (with-shell-interpreter--hack-connection-local-variables
         `(
           ;; REVIEW: only those props in criteria?
           ;; this is what `shell' uses, but maybe can we do better?
           :application tramp
           :protocol ,(file-remote-p path 'method)
           :user ,(file-remote-p path 'user)
           :machine ,(file-remote-p path 'host)))
        (setq output connection-local-variables-alist))
      output)))



;; HELPERS: STANDARD SHELL VARS

(defun with-shell-interpreter--resolve-shell-var (syms is-remote
                                                       allow-buffer-local-vars
                                                       allow-cnnx-local-vars cnnx-local-vars
                                                       input-value
                                                       &optional fallback match-interpreter interpreter)
  "Resolve a shell variable with standard precedence order.
SYMS is a symbol or list of symbols to look up (tried in order at each level).
IS-REMOTE, ALLOW-BUFFER-LOCAL-VARS, ALLOW-CNNX-LOCAL-VARS, CNNX-LOCAL-VARS
control which scopes are consulted.
INPUT-VALUE, if non-nil, takes highest precedence.
FALLBACK is used if no value is found at any level.
When MATCH-INTERPRETER is non-nil, connection-local values are only used
when INTERPRETER matches the connection-local shell file name.

The order of precedence is:
 - INPUT-VALUE
 - buffer-local value (if ALLOW-BUFFER-LOCAL-VARS)
 - connection-local value (if ALLOW-CNNX-LOCAL-VARS)
 - global value
 - FALLBACK"
  (let ((syms (if (listp syms) syms (list syms))))
    (or input-value
        ;; buffer-local value
        (when allow-buffer-local-vars
          (cl-some (lambda (sym)
                     (when (with-shell-interpreter--boundp-buffer-local sym)
                       (with-shell-interpreter--symbol-value sym t)))
                   syms))
        ;; connection-local value
        (when (and is-remote
                   allow-cnnx-local-vars
                   (or (not match-interpreter)
                       (string= interpreter (alist-get 'explicit-shell-file-name cnnx-local-vars))
                       (string= interpreter (alist-get 'shell-file-name cnnx-local-vars))))
          (cl-some (lambda (sym) (alist-get sym cnnx-local-vars)) syms))
        ;; global value
        (cl-some (lambda (sym)
                   (ignore-errors
                     (with-shell-interpreter--symbol-value sym nil)))
                 syms)
        ;; universal fallback
        fallback)))


(defun with-shell-interpreter--interpreter-value (is-remote
                                                  &optional allow-buffer-local-vars
                                                  allow-cnnx-local-vars cnnx-local-vars
                                                  input-value)
  "Determine value of shell interpreter.
Delegates to `with-shell-interpreter--resolve-shell-var' then normalizes the path."
  (with-shell-interpreter--normalize-path
   (with-shell-interpreter--resolve-shell-var
    '(explicit-shell-file-name shell-file-name)
    is-remote allow-buffer-local-vars allow-cnnx-local-vars cnnx-local-vars
    input-value "/usr/bin/sh")))



;; HELPER: COMPUTED VARS

(defun with-shell-interpreter--generate-props (path interpreter allow-local-vars)
  "Generate several usefull variable values from PATH, INTERPRETER and ALLOW-LOCAL-VARS.
This function exists to be reused by package `friendly-shell'."
  (unless path
    (setq path default-directory))
  (unless (file-exists-p path)
    (error "Path %s doesn't seem to exist" path))

  (let* ((is-remote (file-remote-p path))
         (allow-local-vars (or allow-local-vars 'connection))
         (allow-buffer-local-vars  (member allow-local-vars '(buffer both)))
         (allow-cnnx-local-vars (member allow-local-vars '(connection both)))
         (cnnx-local-vars (with-shell-interpreter--cnnx-local-vars path))
         (interpreter (with-shell-interpreter--interpreter-value is-remote
                                                                 allow-buffer-local-vars
                                                                 allow-cnnx-local-vars cnnx-local-vars
                                                                 interpreter))
         (interpreter-name (with-shell-interpreter--interpreter-name interpreter))
         (explicit-interpreter-args-var (intern (concat "explicit-" interpreter-name "-args"))))
    (list path is-remote
          allow-buffer-local-vars
          allow-cnnx-local-vars cnnx-local-vars
          interpreter interpreter-name
          explicit-interpreter-args-var)))




(provide 'with-shell-interpreter)

;;; with-shell-interpreter.el ends here
