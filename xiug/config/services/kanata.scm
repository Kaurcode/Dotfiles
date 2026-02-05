(define-module (xiug config services kanata)
               #:use-module (guix gexp)
               #:use-module (guix records)
               #:use-module (gnu services)
               #:use-module (gnu services base)       ; etc-service-type
               #:use-module (gnu services shepherd)   ; shepherd-root-service-type
               #:use-module (gnu packages rust-apps)
               #:export (kanata-service-type
                         kanata-configuration
                         kanata-configuration?))

(define-record-type* <kanata-configuration>
                     kanata-configuration make-kanata-configuration
                     kanata-configuration?
                     (package 
                      kanata-configuration-package
                      (default kanata))
                     (config-file 
                      kanata-configuration-config-file
                      (default (plain-file "kanata.kbd" "")))
                     (etc-path
                      kanata-configuration-etc-path
                      (default "kanata/kanata.kbd"))
                     (respawn? 
                      kanata-configuration-respawn?
                      (default #t))
                     (log-file 
                      kanata-configuration-log-file
                      (default #f))
                     (extra-arguments
                      kanata-configuration-extra-arguments
                      (default '())))

(define (kanata-etc-service cfg)
  `((,(kanata-configuration-etc-path cfg)
     ,(kanata-configuration-config-file cfg))))


(define (kanata-shepherd-services cfg)
  (let* ((config-path (string-append "/etc/" (kanata-configuration-etc-path cfg)))
         (exe         (file-append (kanata-configuration-package cfg) "/bin/kanata"))
         (respawn?    (kanata-configuration-respawn? cfg))
         (log-file    (kanata-configuration-log-file cfg))
         (extra-args  (kanata-configuration-extra-arguments cfg))
         (args        #~(list #$exe "--cfg" #$config-path #$@extra-args))
         (start       (if log-file
                          #~(make-forkexec-constructor #$args 
                                                       #:log-file #$log-file)
                          #~(make-forkexec-constructor #$args))))
    (list
      (shepherd-service 
        (provision '(kanata))
        (requirement '(file-systems udev user-processes))
        (respawn? respawn?)
        (start start)
        (stop #~(make-kill-destructor))))))

(define kanata-service-type
  (service-type
    (name 'kanata)
    (extensions
      (list (service-extension shepherd-root-service-type 
                               kanata-shepherd-services)
            (service-extension etc-service-type 
                               kanata-etc-service)))
    (default-value (kanata-configuration))
    (description "Run the kanata keyboard daemon as a system service.")))

