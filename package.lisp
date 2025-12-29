;;;; package.lisp

(defpackage #:plclib-cl-web
  (:use #:cl)
  (:export
   ;; Main entry point
   #:start-server
   #:stop-server

   ;; PLC state management
   #:reset-plc
   #:get-plc-state
   #:toggle-input
   #:get-outputs

   ;; Program management
   #:load-program
   #:save-program
   #:get-current-program

   ;; Execution control
   #:run-program
   #:stop-program
   #:step-program

   ;; IL parsing and conversion
   #:parse-il
   #:il-to-ladder
   #:render-ladder-svg))
