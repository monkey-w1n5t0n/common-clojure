;;;; SBCL Compiler Configuration
;;;; Enables strict type checking and comprehensive warnings

;; Maximum safety and debug information
(declaim (optimize (safety 3)   ; Full runtime type checking
                   (debug 3)    ; Maximum debug info
                   (speed 0)))  ; Don't sacrifice safety for speed

;; Show compiler optimization notes (normally suppressed)
;; These hint at type ambiguities that prevent optimization
(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))

;; Treat style warnings seriously during development
;; SBCL's style warnings indicate code the compiler can't fully understand
