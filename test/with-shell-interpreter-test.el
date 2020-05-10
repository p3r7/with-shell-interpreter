

(require 'ert)
(require 'cl-lib)

(require 'with-shell-interpreter)



;; DATA SET

(defconst with-shell-interpreter--test-cases
  '((
     :case "INPUT ONLY"
     :allow-local-vars connection
     :buffer-local-vars ()
     :cnnx-local-vars ()
     :input (
             :path "/ssh:pi@raspberry:/home/pi"
             :interpreter "/bin/input")
     :expected (
                :interpreter "/bin/input"))
    ;; INPUT > CNNX
    (
     :case "INPUT > CNNX"
     :allow-local-vars connection
     :buffer-local-vars ()
     :cnnx-local-vars (((explicit-cnnx-args "-c" "export EMACS=; export TERM=dumb; export RPI=; stty echo; bash") (explicit-shell-file-name . "/bin/cnnx")))
     :input (
             :path "/ssh:pi@raspberry:/home/pi"
             :interpreter "/bin/input")
     :expected (
                :interpreter "/bin/input"))
    ;; INPUT > BUFFER
    (
     :case "INPUT > BUFFER"
     :allow-local-vars buffer
     :buffer-local-vars ((explicit-shell-file-name . "/bin/buffer"))
     :cnnx-local-vars ()
     :input (
             :path "/ssh:pi@raspberry:/home/pi"
             :interpreter "/bin/input")
     :expected (
                :interpreter "/bin/input"))
    ;; BUFFER > CNNX
    (
     :case "BUFFER > CNNX"
     :allow-local-vars both
     :buffer-local-vars ((explicit-shell-file-name . "/bin/buffer"))
     :cnnx-local-vars ((explicit-shell-file-name . "/bin/cnnx"))
     :input (
             :path "/ssh:pi@raspberry:/home/pi"
             :interpreter nil)
     :expected (
                :interpreter "/bin/buffer"))
    )
  "List test cases.")




;; TESTS

(ert-deftest with-shell-interpreter--interpreter-value-test ()
  "Ensure proper interpreter value."
  (mapc (lambda (item)
          (let* ((t-case (plist-get item :case))
                 ;; local vars
                 (allow-local-vars (plist-get item :allow-local-vars))
                 (allow-buffer-local-vars (member allow-local-vars '(buffer both)))
                 (allow-cnnx-local-vars (member allow-local-vars '(connection both)))
                 (buffer-local-vars (plist-get item :buffer-local-vars))
                 (cnnx-local-vars (plist-get item :cnnx-local-vars))
                 ;; path
                 (path (plist-get (plist-get item :input) :path))
                 ;; input
                 (interpreter (plist-get (plist-get item :input) :interpreter))
                 ;; output
                 (expected (plist-get (plist-get item :expected) :interpreter))
                 result)

            (with-temp-buffer
              (dolist (e buffer-local-vars)
                (let ((k (car e))
                      (v (cdr e)))
                  (make-local-variable k)
                  (set k v)))

              (setq result (with-shell-interpreter--interpreter-value
                            (file-remote-p path)
                            allow-buffer-local-vars
                            allow-cnnx-local-vars cnnx-local-vars
                            interpreter))

              ;; NB: using eval/quote to get better output in ERT
              (eval
               `(should
                 (and ,t-case
                      (string= ,result ,expected))
                 )))))
        with-shell-interpreter--test-cases))




;;; friendly-tramp-path-test.el ends here
