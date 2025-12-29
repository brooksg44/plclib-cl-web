;;;; src/web-server.lisp

(in-package #:plclib-cl-web)

;;; Web server using Hunchentoot

(defparameter *server* nil "Hunchentoot server instance")
(defparameter *server-port* 8080 "Web server port")

;;; JSON utilities

(defun json-response (data)
  "Create JSON response with proper headers"
  (setf (hunchentoot:content-type*) "application/json")
  (json:encode-json-to-string data))

(defun json-error (message)
  "Create JSON error response"
  (json-response (list (cons :error message) (cons :success :false))))

(defun json-success (data)
  "Create JSON success response"
  (json-response (cons (cons :success t) data)))

;;; Request handlers

(hunchentoot:define-easy-handler (index :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (with-open-file (stream (merge-pathnames "static/index.html" 
                                           (asdf:system-source-directory :plclib-cl-web))
                          :direction :input
                          :if-does-not-exist nil)
    (if stream
        (let ((html (make-string (file-length stream))))
          (read-sequence html stream)
          html)
        "<html><body><h1>Error: index.html not found</h1></body></html>")))

(hunchentoot:define-easy-handler (load-program-handler :uri "/api/load-program") (program)
  (handler-case
      (progn
        (set-program program)
        (let ((parsed (parse-and-validate-il program)))
          (set-parsed-program parsed)
          (json-success (list (cons :message "Program loaded successfully")
                              (cons :rung-count (length (split-into-rungs parsed)))))))
    (error (e)
      (json-error (format nil "~A" e)))))

(hunchentoot:define-easy-handler (get-state-handler :uri "/api/state") ()
  (let ((state (get-plc-state)))
    (json-success (list (cons :inputs (getf state :inputs))
                        (cons :outputs (getf state :outputs))
                        (cons :execution-state (string-downcase (symbol-name (getf state :execution-state))))
                        (cons :scan-count (getf state :scan-count))))))

(hunchentoot:define-easy-handler (toggle-input-handler :uri "/api/toggle-input") (id)
  (handler-case
      (let ((input-id (parse-integer id)))
        (toggle-input input-id)
        (json-success (list (cons :message "Input toggled")
                            (cons :input-id input-id)
                            (cons :value (get-input input-id)))))
    (error (e)
      (json-error (format nil "~A" e)))))

(hunchentoot:define-easy-handler (run-handler :uri "/api/run") ()
  (handler-case
      (progn
        (start-continuous-execution)
        (json-success (list (cons :message "Program running"))))
    (error (e)
      (json-error (format nil "~A" e)))))

(hunchentoot:define-easy-handler (stop-handler :uri "/api/stop") ()
  (handler-case
      (progn
        (stop-continuous-execution)
        (json-success (list (cons :message "Program stopped"))))
    (error (e)
      (json-error (format nil "~A" e)))))

(hunchentoot:define-easy-handler (step-handler :uri "/api/step") ()
  (handler-case
      (progn
        (execute-single-step)
        (json-success (list (cons :message "Executed one step")
                            (cons :scan-count (get-scan-count)))))
    (error (e)
      (json-error (format nil "~A" e)))))

(hunchentoot:define-easy-handler (reset-handler :uri "/api/reset") ()
  (handler-case
      (progn
        (stop-continuous-execution)
        (reset-plc)
        (json-success (list (cons :message "PLC reset"))))
    (error (e)
      (json-error (format nil "~A" e)))))

(hunchentoot:define-easy-handler (get-ladder-handler :uri "/api/ladder") ()
  (setf (hunchentoot:content-type*) "image/svg+xml")
  (handler-case
      (render-current-program-svg)
    (error (e)
      (format nil "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"800\" height=\"200\"><text x=\"400\" y=\"100\" text-anchor=\"middle\" font-size=\"16\">Error: ~A</text></svg>" e))))

(hunchentoot:define-easy-handler (get-program-handler :uri "/api/program") ()
  (json-success (list (cons :program (get-program)))))

(hunchentoot:define-easy-handler (save-program-handler :uri "/api/save-program") (filepath)
  (handler-case
      (progn
        (save-program filepath)
        (json-success (list (cons :message "Program saved")
                            (cons :filepath filepath))))
    (error (e)
      (json-error (format nil "~A" e)))))

(hunchentoot:define-easy-handler (download-program-handler :uri "/api/download") ()
  (setf (hunchentoot:content-type*) "application/octet-stream")
  (setf (hunchentoot:header-out "Content-Disposition") 
        "attachment; filename=\"program.il\"")
  (get-program))

(hunchentoot:define-easy-handler (upload-program-handler :uri "/api/upload") ()
  (handler-case
      (let ((file (hunchentoot:post-parameter "file")))
        (if file
            (let ((content (hunchentoot:post-parameter "file")))
              (set-program content)
              (let ((parsed (parse-and-validate-il content)))
                (set-parsed-program parsed)
                (json-success (list (cons :message "Program uploaded successfully")
                                    (cons :rung-count (length (split-into-rungs parsed)))))))
            (json-error "No file uploaded")))
    (error (e)
      (json-error (format nil "~A" e)))))

(hunchentoot:define-easy-handler (list-examples-handler :uri "/api/examples") ()
  (handler-case
      (let* ((examples-dir (merge-pathnames "examples/" 
                                            (asdf:system-source-directory :plclib-cl-web)))
             (files (directory (merge-pathnames "*.il" examples-dir)))
             (example-list (mapcar (lambda (file)
                                     (list (cons :name (file-namestring file))
                                           (cons :path (namestring file))))
                                   files)))
        (json-success (list (cons :examples example-list))))
    (error (e)
      (json-error (format nil "~A" e)))))

(hunchentoot:define-easy-handler (load-example-handler :uri "/api/load-example") (filename)
  (handler-case
      (let* ((examples-dir (merge-pathnames "examples/" 
                                            (asdf:system-source-directory :plclib-cl-web)))
             (filepath (merge-pathnames filename examples-dir)))
        (if (probe-file filepath)
            (let ((program (load-program-from-file (namestring filepath))))
              (set-program program)
              (let ((parsed (parse-and-validate-il program)))
                (set-parsed-program parsed)
                (json-success (list (cons :message "Example loaded successfully")
                                    (cons :program program)
                                    (cons :rung-count (length (split-into-rungs parsed)))))))
            (json-error (format nil "Example file not found: ~A" filename))))
    (error (e)
      (json-error (format nil "~A" e)))))

;;; Static file serving

(defun setup-static-files ()
  "Setup static file serving for CSS and JS"
  (push (hunchentoot:create-folder-dispatcher-and-handler
         "/static/" 
         (merge-pathnames "static/" (asdf:system-source-directory :plclib-cl-web)))
        hunchentoot:*dispatch-table*))

(defun setup-examples-serving ()
  "Setup serving of example files"
  (push (hunchentoot:create-folder-dispatcher-and-handler
         "/examples/" 
         (merge-pathnames "examples/" (asdf:system-source-directory :plclib-cl-web)))
        hunchentoot:*dispatch-table*))

;;; Server control

(defun start-server (&optional (port *server-port*))
  "Start the web server"
  (when *server*
    (stop-server))
  
  ;; Initialize PLC
  (initialize-plc)
  
  ;; Setup static files
  (setup-static-files)
  (setup-examples-serving)
  
  ;; Start server
  (setf *server* (make-instance 'hunchentoot:easy-acceptor :port port))
  (hunchentoot:start *server*)
  (format t "~%PLC Simulator web server started on port ~D~%" port)
  (format t "Open http://localhost:~D in your browser~%~%" port))

(defun stop-server ()
  "Stop the web server"
  (when *server*
    (stop-continuous-execution)
    (hunchentoot:stop *server*)
    (setf *server* nil)
    (format t "~%PLC Simulator web server stopped~%~%")))
