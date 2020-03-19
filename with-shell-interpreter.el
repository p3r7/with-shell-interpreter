;;; with-shell-interpreter.el --- Helper for shell command APIs -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/with-shell-interpreter
;; Package-Requires: ((cl-lib "0.6.1"))
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;  -----------
;;
;; Helper macro for Emacs shell command APIs, making implicit argument as explicit keyword arguments.
;; Provides macro `with-shell-interpreter'.
;;
;; For detailed instructions, please look at the README.md at https://github.com/p3r7/with-shell-interpreter/blob/master/README.md

;;; Code:



;; REQUIRES

(require 'cl-lib)

(require 'shell)



;; VARS

(defvar with-shell-interpreter-default-remote "/bin/bash"
  "For remote shells, default interpreter exec to fallback to if :interpreter \
is not specified.
Let-binds `explicit-shell-file-name' and `shell-file-name'.")
(defvar with-shell-interpreter-default-remote-args '("-c" "export EMACS=; export TERM=dumb; stty echo; bash")
  "For remote shells, default interpreter args to fallback to if \
:interpreter-args is not specified and :interpreter is equal to \
`with-shell-interpreter-default-remote'.
Let-binds `explicit-INTEPRETER-args'")
(defvar with-shell-interpreter-default-remote-command-swith "-c"
  "For remote shells, default interpreter command switch to fallback to if \
:command-switch is not specified.
Let-binds `shell-command-switch'")

;; NB: only bound on Windows build of Emacs
(unless (boundp 'w32-quote-process-args)
  ;; tame lexical binding warnings
  (defvar w32-quote-process-args))



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
                    shell command (e.g. \"-c\" in bourne shell and most
                    derivatives).
                    Let-binds `shell-command-switch'.
                    Useful only for single shell commands.
:w32-arg-quote      Only affecting Microsoft Windows build of Emacs.
                    Character to use for quoting arguments.
                    Let-binds `w32-quote-process-args'.
:allow-local-vars   If t, allow local values to have precedence over global ones for:
                     - `explicit-shell-file-name'
                     - `explicit-INTEPRETER-args'
                     - `shell-command-switch'
                     - `w32-quote-process-args'

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
  (unless path
    (setq path default-directory))
  (unless (file-exists-p path)
    (error "Path %s doesn't seem to exist" path))

  (let* ((func
          (if (functionp form) form
            ;; Try to use the "current" lexical/dynamic mode for `form'.
            (eval `(lambda () ,form) lexical-binding)))
         (is-remote (file-remote-p path))
         (ignore-local-vars (not allow-local-vars))
         (interpreter (with-shell-interpreter--get-interpreter-value is-remote ignore-local-vars interpreter))
         (interpreter-name (with-shell-interpreter--get-interpreter-name interpreter))
         (explicit-interpreter-args-var (intern (concat "explicit-" interpreter-name "-args")))
         (interpreter-args (with-shell-interpreter--get-interpreter-args-value is-remote explicit-interpreter-args-var
                                                                               interpreter
                                                                               ignore-local-vars interpreter-args))
         (command-switch (with-shell-interpreter--get-command-switch is-remote ignore-local-vars command-switch))
         ;; bellow are vars acting as implicit options to shell functions
         (default-directory path)
         (shell-file-name interpreter)
         (explicit-shell-file-name interpreter)
         (shell-command-switch command-switch)
         ;; NB: w32-only feature
         (w32-quote-process-args (with-shell-interpreter--get-w32-quote-process-args ignore-local-vars w32-arg-quote)))
    (cl-progv
        (list explicit-interpreter-args-var)
        (list interpreter-args)
      (funcall func))))



;; PRIVATE HELPERS

(defun with-shell-interpreter--normalize-path (path)
  "Normalize PATH, converting \\ into /."
  ;; REVIEW: shouldn't we just use instead `convert-standard-filename'
  ;; or even `executable-find'?
  (subst-char-in-string ?\\ ?/ path))


(defun with-shell-interpreter--get-interpreter-name (interpreter)
  "Extracts INTERPRETER name, keeping extension."
  (file-name-nondirectory interpreter))


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


(defun with-shell-interpreter--symbol-value (sym &optional ignore-local)
  "Return the value of SYM in current buffer.
If IGNORE-LOCAL is nil, returns global value."
  (if ignore-local
      ;; NB: if local-only `default-value' throws an error
      (ignore-errors
        (default-value sym))
    (symbol-value sym)))


(defun with-shell-interpreter--get-interpreter-value (is-remote &optional ignore-local-vars input-value)
  "Determine value of shell interpreter.
Uses INPUT-VALUE if not empty, else fallbacks to default values, depending on
whether:
 - IS-REMOTE or not
 - IGNORE-LOCAL-VARS or not"
  (with-shell-interpreter--normalize-path
   (or input-value
       (if is-remote
           with-shell-interpreter-default-remote
         (or (with-shell-interpreter--symbol-value 'shell-file-name ignore-local-vars)
             (with-shell-interpreter--symbol-value 'explicit-shell-file-name ignore-local-vars))))))


(defun with-shell-interpreter--get-interpreter-args-value (is-remote args-var-name interpreter &optional ignore-local-vars input-value)
  "Determine value of shell interpreter.
Uses INPUT-VALUE if not empty, else fallbacks to default values, depending on
ARGS-VAR-NAME, INTERPRETER and whether:
 - IS-REMOTE or not
 - IGNORE-LOCAL-VARS or not"
  (or input-value
      (when (and is-remote
                 (string= interpreter with-shell-interpreter-default-remote))
        with-shell-interpreter-default-remote-args)
      (when (boundp args-var-name)
        (with-shell-interpreter--symbol-value args-var-name ignore-local-vars))))


(defun with-shell-interpreter--get-command-switch (is-remote &optional ignore-local-vars input-value)
  "Determine value of shell command switch.
Uses INPUT-VALUE if not empty, else fallbacks to default values, depending on
 whether:
 - IS-REMOTE or not
 - IGNORE-LOCAL-VARS or not"
  (or input-value
      (if is-remote
          with-shell-interpreter-default-remote-command-swith
        (with-shell-interpreter--symbol-value 'shell-command-switch ignore-local-vars))))


(defun with-shell-interpreter--get-w32-quote-process-args (&optional ignore-local-vars input-value)
  "Determine value of shell command switch.
Uses INPUT-VALUE if not empty, else fallbacks to default values, depending on
whether:
 - IGNORE-LOCAL-VARS or not"
  (or input-value
      (when (boundp 'w32-quote-process-args)
        (with-shell-interpreter--symbol-value 'w32-quote-process-args ignore-local-vars))))




(provide 'with-shell-interpreter)

;;; with-shell-interpreter.el ends here
