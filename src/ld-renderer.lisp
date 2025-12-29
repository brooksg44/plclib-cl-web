;;;; src/ld-renderer.lisp

(in-package #:plclib-cl-web)

;;; SVG Ladder Diagram Renderer - LDMicro style

(defparameter *element-width* 80 "Width of a ladder element in pixels")
(defparameter *element-height* 50 "Height of a ladder element in pixels")
(defparameter *rail-margin* 20 "Margin for power rails")
(defparameter *rung-spacing* 80 "Vertical spacing between rungs")
(defparameter *contact-width* 60 "Width of contact symbol")
(defparameter *contact-height* 30 "Height of contact symbol")
(defparameter *coil-width* 60 "Width of coil symbol")
(defparameter *coil-height* 30 "Height of coil symbol")

;;; Device state helpers

(defun parse-device-string (device-str)
  "Parse device string 'X0' -> (type . number)"
  (when (and device-str (> (length device-str) 0))
        (let ((device-type (char device-str 0))
              (device-num-str (subseq device-str 1)))
          (handler-case
              (cons device-type (parse-integer device-num-str))
            (error () nil)))))

(defun get-device-state (device-str)
  "Get the current state (0 or 1) of a device"
  (let ((device (parse-device-string device-str)))
    (when device
          (let ((device-type (car device))
                (device-num (cdr device)))
            (case device-type
              (#\X (get-input device-num))
              (#\Y (get-output device-num))
              (#\M (get-relay (format nil "M~D" device-num)))
              (#\T (get-relay (format nil "T~D" device-num)))
              (#\C (get-relay (format nil "C~D" device-num)))
              (t 0))))))

(defun element-color (device-str element-type)
  "Determine color for element based on its state"
  (let ((state (get-device-state device-str)))
    (if (and state (= state 1))
        (case element-type
          (:contact-no "red") ; NO contact energized
          (:contact-nc "black") ; NC contact de-energized
          ((:coil :set :reset) "red") ; Coil energized
          (t "black"))
        (case element-type
          (:contact-no "black") ; NO contact not energized
          (:contact-nc "red") ; NC contact conducting
          ((:coil :set :reset) "black") ; Coil not energized
          (t "black")))))

;;; SVG Generation Helpers

(defun svg-line (x1 y1 x2 y2 &optional (stroke "black") (stroke-width 2))
  "Generate SVG line element"
  (format nil "<line x1=\"~D\" y1=\"~D\" x2=\"~D\" y2=\"~D\" stroke=\"~A\" stroke-width=\"~D\"/>"
    x1 y1 x2 y2 stroke stroke-width))

(defun svg-rect (x y width height &optional (fill "none") (stroke "black") (stroke-width 2))
  "Generate SVG rectangle element"
  (format nil "<rect x=\"~D\" y=\"~D\" width=\"~D\" height=\"~D\" fill=\"~A\" stroke=\"~A\" stroke-width=\"~D\"/>"
    x y width height fill stroke stroke-width))

(defun svg-text (x y text &optional (font-size 14))
  "Generate SVG text element"
  (format nil "<text x=\"~D\" y=\"~D\" font-family=\"monospace\" font-size=\"~D\" text-anchor=\"middle\" dominant-baseline=\"middle\">~A</text>"
    x y font-size text))

(defun svg-circle (cx cy r &optional (fill "none") (stroke "black") (stroke-width 2))
  "Generate SVG circle element"
  (format nil "<circle cx=\"~D\" cy=\"~D\" r=\"~D\" fill=\"~A\" stroke=\"~A\" stroke-width=\"~D\"/>"
    cx cy r fill stroke stroke-width))

;;; Render individual LD elements

(defun render-contact-no (x y device)
  "Render normally open contact --] [--"
  (let ((left-x (- x (/ *contact-width* 2)))
        (right-x (+ x (/ *contact-width* 2)))
        (top-y (- y (/ *contact-height* 2)))
        (bottom-y (+ y (/ *contact-height* 2)))
        (color (element-color device :contact-no)))
    (with-output-to-string (s)
      ;; Left horizontal line
      (format s "~A~%" (svg-line (- left-x 20) y left-x y color))
      ;; Right horizontal line
      (format s "~A~%" (svg-line right-x y (+ right-x 20) y color))
      ;; Vertical lines for contact
      (format s "~A~%" (svg-line left-x top-y left-x bottom-y color))
      (format s "~A~%" (svg-line right-x top-y right-x bottom-y color))
      ;; Top and bottom horizontal lines
      (format s "~A~%" (svg-line left-x top-y right-x top-y color))
      (format s "~A~%" (svg-line left-x bottom-y right-x bottom-y color))
      ;; Device label
      (format s "~A~%" (svg-text x (- top-y 10) device 12)))))

(defun render-contact-nc (x y device)
  "Render normally closed contact --]/[--"
  (let ((left-x (- x (/ *contact-width* 2)))
        (right-x (+ x (/ *contact-width* 2)))
        (top-y (- y (/ *contact-height* 2)))
        (bottom-y (+ y (/ *contact-height* 2)))
        (color (element-color device :contact-nc)))
    (with-output-to-string (s)
      ;; Left horizontal line
      (format s "~A~%" (svg-line (- left-x 20) y left-x y color))
      ;; Right horizontal line
      (format s "~A~%" (svg-line right-x y (+ right-x 20) y color))
      ;; Vertical lines for contact
      (format s "~A~%" (svg-line left-x top-y left-x bottom-y color))
      (format s "~A~%" (svg-line right-x top-y right-x bottom-y color))
      ;; Top and bottom horizontal lines
      (format s "~A~%" (svg-line left-x top-y right-x top-y color))
      (format s "~A~%" (svg-line left-x bottom-y right-x bottom-y color))
      ;; Diagonal line for NC contact
      (format s "~A~%" (svg-line left-x top-y right-x bottom-y color))
      ;; Device label
      (format s "~A~%" (svg-text x (- top-y 10) device 12)))))

(defun render-coil (x y device)
  "Render output coil ( )"
  (let ((left-x (- x (/ *coil-width* 2)))
        (right-x (+ x (/ *coil-width* 2)))
        (color (element-color device :coil)))
    (with-output-to-string (s)
      ;; Left horizontal line
      (format s "~A~%" (svg-line (- left-x 20) y left-x y color))
      ;; Right horizontal line
      (format s "~A~%" (svg-line right-x y (+ right-x 20) y color))
      ;; Coil circles
      (format s "~A~%" (svg-circle left-x y 15 "none" color))
      (format s "~A~%" (svg-circle right-x y 15 "none" color))
      ;; Device label
      (format s "~A~%" (svg-text x (+ y 30) device 12)))))

(defun render-set-coil (x y device)
  "Render SET coil (S)"
  (let ((left-x (- x (/ *coil-width* 2)))
        (right-x (+ x (/ *coil-width* 2)))
        (color (element-color device :set)))
    (with-output-to-string (s)
      ;; Left horizontal line
      (format s "~A~%" (svg-line (- left-x 20) y left-x y color))
      ;; Right horizontal line
      (format s "~A~%" (svg-line right-x y (+ right-x 20) y color))
      ;; Coil circles
      (format s "~A~%" (svg-circle left-x y 15 "none" color))
      (format s "~A~%" (svg-circle right-x y 15 "none" color))
      ;; S marker
      (format s "~A~%" (svg-text x y "S" 14))
      ;; Device label
      (format s "~A~%" (svg-text x (+ y 30) device 12)))))

(defun render-reset-coil (x y device)
  "Render RESET coil (R)"
  (let ((left-x (- x (/ *coil-width* 2)))
        (right-x (+ x (/ *coil-width* 2)))
        (color (element-color device :reset)))
    (with-output-to-string (s)
      ;; Left horizontal line
      (format s "~A~%" (svg-line (- left-x 20) y left-x y color))
      ;; Right horizontal line
      (format s "~A~%" (svg-line right-x y (+ right-x 20) y color))
      ;; Coil circles
      (format s "~A~%" (svg-circle left-x y 15 "none" color))
      (format s "~A~%" (svg-circle right-x y 15 "none" color))
      ;; R marker
      (format s "~A~%" (svg-text x y "R" 14))
      ;; Device label
      (format s "~A~%" (svg-text x (+ y 30) device 12)))))

(defun render-element (element x y)
  "Render a ladder diagram element at specified position"
  (case (ld-element-type element)
    (:contact-no (render-contact-no x y (ld-element-device element)))
    (:contact-nc (render-contact-nc x y (ld-element-device element)))
    (:coil (render-coil x y (ld-element-device element)))
    (:set (render-set-coil x y (ld-element-device element)))
    (:reset (render-reset-coil x y (ld-element-device element)))
    (t "")))

;;; Render complete rung

(defun render-rung (rung start-y rail-left rail-right)
  "Render a complete ladder rung"
  (let ((elements (ld-rung-elements rung))
        (rung-y start-y)
        (has-branches (ld-rung-branches rung)))
    (with-output-to-string (s)
      ;; Rung number
      (format s "~A~%" (svg-text (- rail-left 40) rung-y
                                 (format nil "~D" (ld-rung-number rung)) 12))

      ;; Track row positions for parallel branches
      (let ((row-positions (make-hash-table :test 'equal))
            (max-row 0)
            (last-col 0))

        ;; Calculate positions and find max row, separate contacts from coils
        (let ((last-contact-col 0)
              (coil-elements nil))
          (dolist (element elements)
            (let ((row (or (ld-element-row element) 0))
                  (col (or (ld-element-col element) 0))
                  (type (ld-element-type element)))
              (setf max-row (max max-row row))
              (setf last-col (max last-col col))
              (let ((x (+ rail-left 40 (* col *element-width*)))
                    (y (+ rung-y (* row 40))))
                (setf (gethash (list row col) row-positions) (cons x y)))
              ;; Track contacts vs coils
              (if (member type '(:coil :set :reset))
                  (push element coil-elements)
                  (when (> row 0)
                        (setf last-contact-col (max last-contact-col col))))))

          ;; Draw vertical branch connections if there are branches
          (when (and has-branches (> max-row 0))
                (let ((branch-x (+ rail-left 40)))
                  ;; Draw vertical line connecting parallel branches at start
                  (format s "~A~%" (svg-line branch-x rung-y branch-x
                                             (+ rung-y (* max-row 40)) "black" 2))
                  ;; Draw vertical line at end reconnecting branches (after last contact)
                  (let ((end-x (+ rail-left 40 (* (1+ last-contact-col) *element-width*))))
                    (format s "~A~%" (svg-line end-x rung-y end-x
                                               (+ rung-y (* max-row 40)) "black" 2))))))

        ;; Render all elements
        (dolist (element elements)
          (let* ((row (or (ld-element-row element) 0))
                 (col (or (ld-element-col element) 0))
                 (pos (gethash (list row col) row-positions)))
            (when pos
                  (let ((x (car pos))
                        (y (cdr pos)))
                    (format s "~A" (render-element element x y))))))

        ;; Connect to right rail from the first row
        (let ((end-x (+ rail-left 40 (* (1+ last-col) *element-width*))))
          (format s "~A~%" (svg-line end-x rung-y rail-right rung-y)))))))

;;; Main SVG rendering function

(defun render-ladder-svg (rungs)
  "Render complete ladder diagram as SVG"
  (let* ((rail-left *rail-margin*)
         (rail-right (- 800 *rail-margin*))
         ;; Calculate height accounting for parallel branches
         (total-height (+ 100
                          (loop for rung in rungs
                                  sum (if (ld-rung-branches rung)
                                          (+ *rung-spacing* 40) ; Extra space for branches
                                          *rung-spacing*))))
         (svg-height (max 200 total-height)))

    (with-output-to-string (s)
      ;; SVG header
      (format s "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"800\" height=\"~D\" viewBox=\"0 0 800 ~D\">~%"
        svg-height svg-height)
      (format s "<rect width=\"800\" height=\"~D\" fill=\"white\"/>~%" svg-height)

      ;; Left power rail
      (format s "~A~%" (svg-line rail-left 40 rail-left (- svg-height 40) "blue" 3))

      ;; Right power rail
      (format s "~A~%" (svg-line rail-right 40 rail-right (- svg-height 40) "blue" 3))

      ;; Render each rung
      (let ((current-y 60))
        (dolist (rung rungs)
          ;; Connection to left rail
          (format s "~A~%" (svg-line rail-left current-y (+ rail-left 40) current-y))
          ;; Rung content
          (format s "~A" (render-rung rung current-y rail-left rail-right))
          ;; Increment Y with extra space for branches
          (incf current-y (if (ld-rung-branches rung)
                              (+ *rung-spacing* 40)
                              *rung-spacing*))))

      ;; SVG footer
      (format s "</svg>~%"))))

;;; Public API

(defun render-current-program-svg ()
  "Render SVG for current program's ladder diagram"
  (let ((rungs (get-ladder-diagram)))
    (if rungs
        (render-ladder-svg rungs)
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"800\" height=\"200\"><text x=\"400\" y=\"100\" text-anchor=\"middle\" font-size=\"16\">No program loaded</text></svg>")))
