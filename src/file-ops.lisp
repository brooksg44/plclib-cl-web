;;;; src/file-ops.lisp

(in-package #:plclib-cl-web)

;;; File operations for IL programs

(defun load-program-from-file (filepath)
  "Load IL program from file"
  (handler-case
      (with-open-file (stream filepath :direction :input)
        (let ((program-text (make-string (file-length stream))))
          (read-sequence program-text stream)
          program-text))
    (error (e)
      (error "Failed to load program from ~A: ~A" filepath e))))

(defun save-program-to-file (filepath program-text)
  "Save IL program to file"
  (handler-case
      (with-open-file (stream filepath 
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-sequence program-text stream)
        t)
    (error (e)
      (error "Failed to save program to ~A: ~A" filepath e))))

(defun load-program (filepath)
  "Load IL program from file and set as current program"
  (let ((program-text (load-program-from-file filepath)))
    (set-program program-text)
    ;; Parse and validate
    (handler-case
        (let ((parsed (parse-and-validate-il program-text)))
          (set-parsed-program parsed)
          program-text)
      (error (e)
        (set-program "")
        (set-parsed-program nil)
        (error "Failed to parse program: ~A" e)))))

(defun save-program (filepath)
  "Save current IL program to file"
  (let ((program-text (get-program)))
    (if (and program-text (> (length program-text) 0))
        (save-program-to-file filepath program-text)
        (error "No program to save"))))
