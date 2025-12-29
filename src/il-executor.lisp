;;;; src/il-executor.lisp

(in-package #:plclib-cl-web)

;;; IL Executor - executes IL instructions using plclib-cl

(defparameter *accumulator* 0
              "Current accumulator value (0 or 1)")

(defparameter *branch-stack* nil
              "Stack for MPS/MRD/MPP branch operations")

;;; Device value access

(defun get-device-value (device)
  "Get value of a device (X, Y, M, etc.)"
  (let ((device-type (car device))
        (device-num (cdr device)))
    (case device-type
      (#\X (get-input device-num))
      (#\Y (get-output device-num))
      (#\M (get-relay (format nil "M~D" device-num)))
      (#\T (get-relay (format nil "T~D" device-num))) ; Timers as relays for now
      (#\C (get-relay (format nil "C~D" device-num))) ; Counters as relays for now
      (t 0))))

(defun set-device-value (device value)
  "Set value of a device (Y, M, etc.)"
  (let ((device-type (car device))
        (device-num (cdr device)))
    (case device-type
      (#\Y (set-output device-num value))
      (#\M (set-relay (format nil "M~D" device-num) value))
      (#\T (set-relay (format nil "T~D" device-num) value))
      (#\C (set-relay (format nil "C~D" device-num) value))
      (t nil))))

;;; Instruction execution

(defun execute-ld (device inverted)
  "Load device value into accumulator (LD/LDI)"
  (let ((value (get-device-value device)))
    (setf *accumulator* (if inverted (- 1 value) value))))

(defun execute-and (device inverted)
  "AND device value with accumulator (AND/ANDN)"
  (let ((value (get-device-value device)))
    (setf *accumulator*
      (if (and (= *accumulator* 1)
               (= (if inverted (- 1 value) value) 1))
          1
          0))))

(defun execute-or (device inverted)
  "OR device value with accumulator (OR/ORN)"
  (let ((value (get-device-value device)))
    (setf *accumulator*
      (if (or (= *accumulator* 1)
              (= (if inverted (- 1 value) value) 1))
          1
          0))))

(defun execute-xor (device inverted)
  "XOR device value with accumulator (XOR/XORN)"
  (let ((value (get-device-value device)))
    (let ((operand (if inverted (- 1 value) value)))
      (setf *accumulator*
        (if (or (and (= *accumulator* 1) (= operand 0))
                (and (= *accumulator* 0) (= operand 1)))
            1
            0)))))

(defun execute-out (device)
  "Output accumulator to device (OUT)"
  (set-device-value device *accumulator*))

(defun execute-set (device)
  "Set device if accumulator is 1 (SET)"
  (when (= *accumulator* 1)
        (set-device-value device 1)))

(defun execute-rst (device)
  "Reset device if accumulator is 1 (RST)"
  (when (= *accumulator* 1)
        (set-device-value device 0)))

(defun execute-mps ()
  "Memory push - push accumulator onto stack (MPS)"
  (push *accumulator* *branch-stack*))

(defun execute-mrd ()
  "Memory read - read top of stack without popping (MRD)"
  (when *branch-stack*
        (setf *accumulator* (first *branch-stack*))))

(defun execute-mpp ()
  "Memory pop - pop from stack into accumulator (MPP)"
  (when *branch-stack*
        (setf *accumulator* (pop *branch-stack*))))

(defun execute-anb ()
  "AND block - AND accumulator with top of stack and pop (ANB)"
  (when *branch-stack*
        (let ((stack-value (pop *branch-stack*)))
          (setf *accumulator*
            (if (and (= *accumulator* 1) (= stack-value 1))
                1
                0)))))

(defun execute-orb ()
  "OR block - OR accumulator with top of stack and pop (ORB)"
  (when *branch-stack*
        (let ((stack-value (pop *branch-stack*)))
          (setf *accumulator*
            (if (or (= *accumulator* 1) (= stack-value 1))
                1
                0)))))

;;; Main instruction executor

(defun execute-instruction (instruction)
  "Execute a single IL instruction"
  (let ((opcode (getf instruction :opcode))
        (device (getf instruction :device)))
    (case opcode
      (:LD (execute-ld device nil))
      (:LDI (execute-ld device t))
      (:AND (execute-and device nil))
      (:ANDN (execute-and device t))
      (:OR (execute-or device nil))
      (:ORN (execute-or device t))
      (:XOR (execute-xor device nil))
      (:XORN (execute-xor device t))
      (:OUT (execute-out device))
      (:SET (execute-set device))
      (:RST (execute-rst device))
      (:MPS (execute-mps))
      (:MRD (execute-mrd))
      (:MPP (execute-mpp))
      (:ANB (execute-anb))
      (:ORB (execute-orb))
      (t (error "Unknown opcode: ~A" opcode)))))

;;; Program execution

(defun execute-program-scan ()
  "Execute one scan cycle of the current program"
  (let ((instructions (get-parsed-program)))
    (when instructions
          ;; Reset accumulator and stack for new scan
          (setf *accumulator* 0)
          (setf *branch-stack* nil)

          ;; Execute all instructions
          (dolist (instruction instructions)
            (execute-instruction instruction))

          ;; Increment scan count
          (increment-scan-count)

          ;; Call plclib-cl scan
          (plclib-cl:plc-scan))))

(defun start-continuous-execution ()
  "Start continuous program execution in a separate thread"
  (when (eq (get-execution-state) :stopped)
        (set-execution-state :running)
        (setf *execution-thread*
          (bt:make-thread
            (lambda ()
              (loop while (eq (get-execution-state) :running)
                    do
                      (handler-case
                          (execute-program-scan)
                        (error (e)
                          (format t "Execution error: ~A~%" e)
                          (set-execution-state :stopped)))
                      (sleep 0.01))) ; 10ms scan cycle
            :name "plc-execution-thread"))))

(defun stop-continuous-execution ()
  "Stop continuous program execution"
  (when (eq (get-execution-state) :running)
        (set-execution-state :stopped)
        (when (and *execution-thread* (bt:thread-alive-p *execution-thread*))
              (bt:join-thread *execution-thread* :timeout 1.0))
        (setf *execution-thread* nil)))

(defun execute-single-step ()
  "Execute a single scan cycle"
  (when (not (eq (get-execution-state) :running))
        (set-execution-state :single-step)
        (handler-case
            (execute-program-scan)
          (error (e)
            (format t "Execution error: ~A~%" e)))
        (set-execution-state :stopped)))
