;;;; plclib-cl-web.asd

(asdf:defsystem #:plclib-cl-web
  :description "Web-based PLC Simulator with IL/LD support using plclib-cl"
  :author "Gregory Brooks"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:plclib-cl
               #:hunchentoot
               #:cl-ppcre
               #:cl-json
               #:bordeaux-threads)
  :components ((:file "package")
               (:module "src"
                        :components
                        ((:file "plc-state")
                         (:file "il-parser")
                         (:file "il-executor")
                         (:file "il-to-ld")
                         (:file "ld-renderer")
                         (:file "file-ops")
                         (:file "web-server")))))
