;;; with-shell-interpreter.el --- Helper for shell command APIs

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: tramp, shell
;; URL: https://github.com/p3r7/with-shell-interpreter
;;
;; Permission is hereby granted to use and distribute this code, with or
;; without modifications, provided that this copyright notice is copied with
;; it. Like anything else that's free, lusty-explorer.el is provided *as is*
;; and comes with no warranty of any kind, either expressed or implied. In no
;; event will the copyright holder be liable for any damages resulting from
;; the use of this software.

;;; Commentary:
;;  -----------
;;
;; For detailed instructions, please look at the README.md

;;; Code:



;; VARS

(defvar default-local-shell-interpreter shell-file-name)
(defvar default-remote-shell-interpreter "/bin/bash")
(defvar default-remote-shell-interpreter-args '("-c" "export EMACS=; export TERM=dumb; stty echo; bash"))
(defvar default-remote-shell-interpreter-command-swith "-c")



;; MAIN HELPER

(defmacro with-shell-interpreter (&rest args)
  "Eval :form at location described by PATH with INTERPRETER binary.

For full documentation, please see the README file that came with
this file.  Usage:

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
                    If only providint a name, ensure that the executable
                    is present in the PATH.
                    Let-binds `explicit-shell-file-name' and
                    `shell-file-name'.
:interpreter-args   Login args to call interpreter with for login.
                    Let-binds `explicit-INTEPRETER-args'.
                    Usefull only for interactive shells.
:command-switch     Command switch arg for asking interpreter to run a
                    shell command (e.g. \"-c\" in bourne shell and most
                    derivatives).
                    Let-binds `shell-command-switch'.
                    Usefull only for single shell commands.
:w32-arg-quote      Only effecting Microsoft Windows build of Emacs.
                    Character to use for quoting arguments.
                    Let-binds `w32-quote-process-args'."
  (declare (indent 1) (debug t))
  `(eval-with-shell-interpreter
    :form (lambda () ,(plist-get args :form))
    :path ,(plist-get args :path)
    :interpreter ,(plist-get args :interpreter)
    :interpreter-args ,(plist-get args :interpreter-args)
    :command-switch ,(plist-get args :command-switch)
    :w32-arg-quote ,(plist-get args :w32-arg-quote)))


(cl-defun eval-with-shell-interpreter (&key form path
                                            interpreter interpreter-args command-switch
                                            w32-arg-quote)
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
         (interpreter (or interpreter
                          (if is-remote
                              default-remote-shell-interpreter
                            shell-file-name)))
         (interpreter (with-shell-interpreter--normalize-path interpreter))
         (interpreter-name (with-shell-interpreter--get-interpreter-name interpreter))
         (explicit-interpreter-args-var (intern (concat "explicit-" interpreter-name "-args")))
         (interpreter-args (or interpreter-args (when is-remote default-remote-shell-interpreter-args)))
         (command-switch (or command-switch
                             (if is-remote
                                 default-remote-shell-interpreter-command-swith
                               shell-command-switch))))

    ;; REVIEW: used to use `with-temp-buffer' alongside `cd'
    ;; might not be necessary now
    (with-temp-buffer
      ;; (cd path)
      (let ((default-directory path)
            (shell-file-name interpreter)
            (explicit-shell-file-name interpreter)
            (shell-command-switch command-switch)
            ;; NB: w32-only feature
            (w32-quote-process-args (or w32-arg-quote
                                        (when (boundp 'w32-quote-process-args)
                                          w32-quote-process-args))))
        (cl-progv
            (list explicit-interpreter-args-var)
            (list (or interpreter-args
                      (when (boundp explicit-interpreter-args-var)
                        (symbol-value explicit-interpreter-args-var))))
          (funcall func))))))



;; PRIVATE HELPERS

(defun with-shell-interpreter--normalize-path (path)
  "Normalize path, converting \\ into /."
  ;; REVIEW: shouldn't we just useinstead `convert-standard-filename'
  ;; or even `executable-find'?
  (subst-char-in-string ?\\ ?/ path))


(defun with-shell-interpreter--get-interpreter-name (interpreter)
  (file-name-nondirectory interpreter))




(provide 'with-shell-interpreter)

;;; with-shell-interpreter.el ends here.
