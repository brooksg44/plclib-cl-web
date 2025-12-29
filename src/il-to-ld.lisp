;;;; src/il-to-ld.lisp

(in-package #:plclib-cl-web)

;;; IL to Ladder Diagram Converter
;;; Converts parsed IL instructions into a ladder diagram structure

(defstruct ld-element
  "A ladder diagram element (contact or coil)"
  type        ; :contact-no, :contact-nc, :coil, :set, :reset
  device      ; Device reference (X0, Y0, M0, etc.)
  row         ; Row position in the rung
  col         ; Column position in the rung
  )

(defstruct ld-rung
  "A ladder diagram rung"
  number      ; Rung number
  elements    ; List of ld-element structures
  branches    ; List of branch structures for parallel paths
  )

;;; Convert device cons to string

(defun device-to-string (device)
  "Convert device cons (type . number) to string 'X0', 'Y1', etc."
  (when device
    (format nil "~C~D" (car device) (cdr device))))

;;; Identify rungs from IL instructions

(defun is-output-instruction-p (opcode)
  "Check if opcode is an output instruction (ends a rung)"
  (member opcode '(:OUT :SET :RST)))

(defun split-into-rungs (instructions)
  "Split IL instructions into rungs (sequences ending with OUT/SET/RST)"
  (let ((rungs nil)
        (current-rung nil))
    (dolist (instruction instructions)
      (push instruction current-rung)
      (when (is-output-instruction-p (getf instruction :opcode))
        (push (nreverse current-rung) rungs)
        (setf current-rung nil)))
    ;; If there are remaining instructions without output, add them as incomplete rung
    (when current-rung
      (push (nreverse current-rung) rungs))
    (nreverse rungs)))

;;; Convert rung instructions to LD elements

(defun convert-rung-to-ld (rung-instructions rung-number)
  "Convert a rung's IL instructions to LD elements"
  (let ((elements nil)
        (col 0)
        (row 0)
        (branch-row 0)
        (max-col 0)
        (branch-stack nil)
        (has-branches nil))
    
    (dolist (instruction rung-instructions)
      (let ((opcode (getf instruction :opcode))
            (device (getf instruction :device)))
        
        (case opcode
          ;; Load instructions start a new series
          ((:LD :LDI)
           ;; If we've seen MPS, this starts a new branch on a new row
           (when (and has-branches (not (null branch-stack)))
             (setf row (1+ branch-row))
             (setf col 0))
           (push (make-ld-element
                  :type (if (eq opcode :LD) :contact-no :contact-nc)
                  :device (device-to-string device)
                  :row row
                  :col col)
                 elements)
           (incf col)
           (setf max-col (max max-col col)))
          
          ;; AND instructions add to series
          ((:AND :ANDN)
           (push (make-ld-element
                  :type (if (eq opcode :AND) :contact-no :contact-nc)
                  :device (device-to-string device)
                  :row row
                  :col col)
                 elements)
           (incf col)
           (setf max-col (max max-col col)))
          
          ;; OR instructions - add on next row at same column
          ((:OR :ORN)
           (incf row)
           (setf col (max 0 (1- col)))
           (push (make-ld-element
                  :type (if (eq opcode :OR) :contact-no :contact-nc)
                  :device (device-to-string device)
                  :row row
                  :col col)
                 elements)
           (incf col)
           (setf max-col (max max-col col))
           (setf has-branches t))
          
          ;; MPS - Push current position to stack
          (:MPS
           (push (list :row row :col col) branch-stack)
           (setf branch-row row)
           (setf has-branches t))
          
          ;; MRD - Read from stack without popping
          (:MRD
           (when branch-stack
             (let ((saved (first branch-stack)))
               (setf row (getf saved :row))
               (setf col (getf saved :col)))))
          
          ;; MPP - Pop from stack
          (:MPP
           (when branch-stack
             (let ((saved (pop branch-stack)))
               (setf row (getf saved :row))
               (setf col (getf saved :col)))))
          
          ;; ANB/ORB - branches merge back to row 0, continue from max column
          ((:ANB :ORB)
           (setf has-branches t)
           (setf row 0)
           (setf col max-col))
          
          ;; Output instructions
          (:OUT
           (push (make-ld-element
                  :type :coil
                  :device (device-to-string device)
                  :row row
                  :col col)
                 elements))
          
          (:SET
           (push (make-ld-element
                  :type :set
                  :device (device-to-string device)
                  :row row
                  :col col)
                 elements))
          
          (:RST
           (push (make-ld-element
                  :type :reset
                  :device (device-to-string device)
                  :row row
                  :col col)
                 elements)))))
    
    (make-ld-rung
     :number rung-number
     :elements (nreverse elements)
     :branches has-branches)))

;;; Main conversion function

(defun il-to-ladder (instructions)
  "Convert IL instructions to ladder diagram structure"
  (let ((rung-instructions (split-into-rungs instructions))
        (rungs nil)
        (rung-number 1))
    
    (dolist (rung-instrs rung-instructions)
      (when rung-instrs
        (push (convert-rung-to-ld rung-instrs rung-number) rungs)
        (incf rung-number)))
    
    (nreverse rungs)))

;;; Utility functions

(defun get-ladder-diagram ()
  "Get ladder diagram for current program"
  (let ((instructions (get-parsed-program)))
    (if instructions
        (il-to-ladder instructions)
        nil)))
