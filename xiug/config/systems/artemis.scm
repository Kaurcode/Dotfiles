;; (define-module (xiug config systems artemis)
;;                #:use-module (xiug config systems base-system)
;;                #:use-module (gnu))

(use-modules (gnu)
             (xiug config systems base-system))

(operating-system
  (inherit base-system))
