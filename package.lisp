;;;; package.lisp

(defpackage #:release-wrangler
  (:use #:cl)
  (:shadowing-import-from #:split-sequence
                          #:split-sequence)
  (:shadowing-import-from #:commando
                          #:run
                          #:run-output-lines
                          #:in-temporary-directory
                          #:with-temporary-directory
                          #:with-posix-cwd))

