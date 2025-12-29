;;;; src/plc-state.lisp

(in-package #:plclib-cl-web)

;;; Global state variables

(defparameter *inputs* (make-array 16 :initial-element 0)
  "16 digital inputs X0-X15")

(defparameter *outputs* (make-array 16 :initial-element 0)
  "16 digital outputs Y0-Y15")

(defparameter *relays* (make-hash-table :test 'equal)
  "Internal relays M0-M99")

(defparameter *current-program* ""
  "Current IL program source code")

(defparameter *parsed-program* nil
  "Parsed IL instructions")

(defparameter *execution-state* :stopped
  "Execution state: :stopped, :running, :single-step")

(defparameter *scan-count* 0
  "Number of scan cycles completed")

(defparameter *execution-thread* nil
  "Thread for continuous execution")

(defparameter *state-lock* (bt:make-lock "plc-state-lock")
  "Lock for thread-safe state access")

;;; Initialization

(defun initialize-plc ()
  "Initialize the PLC system and plclib-cl"
  (bt:with-lock-held (*state-lock*)
    (plclib-cl:plc-init)
    (plclib-cl:serial-begin)
    ;; Set up pin modes for 16 inputs and outputs
    (loop for i from 0 to 15 do
      (plclib-cl:set-pin-mode i :input)
      (plclib-cl:set-pin-mode (+ i 100) :output)) ; Use pins 100-115 for outputs
    (setf *scan-count* 0)
    (setf *execution-state* :stopped)
    (fill *inputs* 0)
    (fill *outputs* 0)
    (clrhash *relays*)
    (setf *current-program* "")
    (setf *parsed-program* nil)))

;;; State access functions

(defun get-input (index)
  "Get input X<index> value (0 or 1)"
  (if (and (>= index 0) (< index 16))
      (aref *inputs* index)
      0))

(defun set-input (index value)
  "Set input X<index> to value (0 or 1)"
  (when (and (>= index 0) (< index 16))
    (bt:with-lock-held (*state-lock*)
      (let ((bit-value (cond
                         ((numberp value) (if (zerop value) 0 1))
                         ((null value) 0)
                         (t 1))))
        (setf (aref *inputs* index) bit-value)
        ;; Update plclib-cl simulation
        (plclib-cl:simulate-input-change index bit-value)))))

(defun get-output (index)
  "Get output Y<index> value (0 or 1)"
  (if (and (>= index 0) (< index 16))
      (aref *outputs* index)
      0))

(defun set-output (index value)
  "Set output Y<index> to value (0 or 1)"
  (when (and (>= index 0) (< index 16))
    (bt:with-lock-held (*state-lock*)
      (let ((bit-value (cond
                         ((numberp value) (if (zerop value) 0 1))
                         ((null value) 0)
                         (t 1))))
        (setf (aref *outputs* index) bit-value)
        ;; Update plclib-cl output
        (plclib-cl:output (+ index 100) bit-value)))))

(defun get-relay (name)
  "Get internal relay M<name> value (0 or 1)"
  (gethash name *relays* 0))

(defun set-relay (name value)
  "Set internal relay M<name> to value (0 or 1)"
  (bt:with-lock-held (*state-lock*)
    (let ((bit-value (cond
                       ((numberp value) (if (zerop value) 0 1))
                       ((null value) 0)
                       (t 1))))
      (setf (gethash name *relays*) bit-value))))

(defun toggle-input (index)
  "Toggle input X<index> between 0 and 1"
  (set-input index (if (zerop (get-input index)) 1 0)))

;;; Program management

(defun set-program (program-text)
  "Set the current IL program"
  (bt:with-lock-held (*state-lock*)
    (setf *current-program* program-text)
    (setf *parsed-program* nil)))

(defun get-program ()
  "Get the current IL program text"
  *current-program*)

(defun set-parsed-program (parsed)
  "Set the parsed program instructions"
  (bt:with-lock-held (*state-lock*)
    (setf *parsed-program* parsed)))

(defun get-parsed-program ()
  "Get the parsed program instructions"
  *parsed-program*)

;;; Execution state management

(defun set-execution-state (state)
  "Set execution state (:stopped, :running, :single-step)"
  (bt:with-lock-held (*state-lock*)
    (setf *execution-state* state)))

(defun get-execution-state ()
  "Get current execution state"
  *execution-state*)

(defun increment-scan-count ()
  "Increment scan cycle counter"
  (bt:with-lock-held (*state-lock*)
    (incf *scan-count*)))

(defun reset-scan-count ()
  "Reset scan cycle counter"
  (bt:with-lock-held (*state-lock*)
    (setf *scan-count* 0)))

(defun get-scan-count ()
  "Get current scan count"
  *scan-count*)

;;; Complete state export

(defun get-plc-state ()
  "Get complete PLC state as a property list"
  (list :inputs (coerce *inputs* 'list)
        :outputs (coerce *outputs* 'list)
        :relays (let ((relay-list nil))
                  (maphash (lambda (k v) (push (cons k v) relay-list)) *relays*)
                  relay-list)
        :execution-state *execution-state*
        :scan-count *scan-count*
        :program *current-program*))

;;; Reset functions

(defun reset-plc ()
  "Reset PLC to initial state"
  (initialize-plc)
  (plclib-cl:plc-reset))
