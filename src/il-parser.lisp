;;;; src/il-parser.lisp

(in-package #:plclib-cl-web)

;;; IL Instruction Parser for IEC 61131-3 subset

(defun trim-string (str)
  "Remove leading and trailing whitespace"
  (string-trim '(#\Space #\Tab #\Newline #\Return) str))

(defun parse-device (device-str)
  "Parse device string like 'X0', 'Y5', 'M10' into (type . number)"
  (let* ((trimmed (trim-string device-str))
         (device-type (char trimmed 0))
         (device-num-str (subseq trimmed 1)))
    (handler-case
        (let ((device-num (parse-integer device-num-str)))
          (cons device-type device-num))
      (error ()
        (error "Invalid device: ~A" device-str)))))

(defun parse-il-line (line line-number)
  "Parse a single IL instruction line, returns instruction structure or NIL for empty/comment lines"
  (let ((trimmed (trim-string line)))
    ;; Skip empty lines and comments
    (when (and (> (length trimmed) 0)
               (not (char= (char trimmed 0) #\;)))
      ;; Split into opcode and operand
      (let* ((parts (cl-ppcre:split "\\s+" trimmed :limit 2))
             (opcode (string-upcase (first parts)))
             (operand (if (second parts) (trim-string (second parts)) nil)))
        (list :line line-number
              :opcode (intern opcode :keyword)
              :operand operand
              :device (when operand (parse-device operand)))))))

(defun parse-il (program-text)
  "Parse IL program text into list of instruction structures"
  (let ((lines (cl-ppcre:split "\\n" program-text))
        (instructions nil)
        (line-number 0))
    (dolist (line lines)
      (incf line-number)
      (handler-case
          (let ((instruction (parse-il-line line line-number)))
            (when instruction
              (push instruction instructions)))
        (error (e)
          (error "Parse error at line ~A: ~A~%Line: ~A" 
                 line-number (princ-to-string e) line))))
    (nreverse instructions)))

;;; Instruction validation

(defparameter *valid-opcodes*
  '(:LD :LDI :ST :STN :AND :ANDN :OR :ORN :XOR :XORN
    :OUT :SET :RST :MPS :MRD :MPP :ANB :ORB)
  "List of valid IL opcodes")

(defun validate-instruction (instruction)
  "Validate an instruction structure"
  (let ((opcode (getf instruction :opcode)))
    (unless (member opcode *valid-opcodes*)
      (error "Invalid opcode at line ~A: ~A" 
             (getf instruction :line) opcode))
    ;; Check if opcode requires operand
    (let ((needs-operand (not (member opcode '(:MPS :MRD :MPP :ANB :ORB)))))
      (when (and needs-operand (null (getf instruction :operand)))
        (error "Opcode ~A requires operand at line ~A"
               opcode (getf instruction :line))))
    instruction))

(defun validate-program (instructions)
  "Validate all instructions in a program"
  (dolist (instruction instructions)
    (validate-instruction instruction))
  instructions)

;;; High-level parsing API

(defun parse-and-validate-il (program-text)
  "Parse and validate IL program, returns instruction list or signals error"
  (let ((instructions (parse-il program-text)))
    (validate-program instructions)
    instructions))
