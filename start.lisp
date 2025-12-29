;;;; start.lisp
;;;; Quick start script for PLC Simulator

(require :asdf)

;; Load Quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" 
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load the system
(format t "~%Loading plclib-cl-web...~%")
(handler-case
    (progn
      (ql:quickload :plclib-cl-web :silent t)
      (format t "~%System loaded successfully!~%"))
  (error (e)
    (format t "~%Error loading system: ~A~%" e)
    (format t "~%Make sure plclib-cl is installed in ~~~~/common-lisp/plclib-cl~%")
    #+sbcl (sb-ext:quit)
    #+ccl (ccl:quit)
    #+clisp (ext:quit)
    #-(or sbcl ccl clisp) (cl-user::quit)))

;; Start the server
(format t "~%Starting PLC Simulator web server...~%")
(plclib-cl-web:start-server)

(format t "~%===============================================~%")
(format t "PLC Simulator is running!~%")
(format t "Open http://localhost:8080 in your browser~%")
(format t "~%To stop the server, evaluate:~%")
(format t "(plclib-cl-web:stop-server)~%")
(format t "===============================================~%~%")
