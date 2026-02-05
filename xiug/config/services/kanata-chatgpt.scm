(define-module (xiug config services kanata-chatgpt)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix profiles)
  #:use-module (gnu services)
  #:use-module (gnu services base)       ; etc-service-type
  #:use-module (gnu services shepherd)   ; shepherd-root-service-type
  #:use-module (gnu packages linux)      ; kmod (for modprobe)
  #:use-module (ice-9 match)
  #:export (kanata-configuration
            kanata-configuration?
            kanata-service-type))

;; Configuration record
(define-record-type* <kanata-configuration>
  kanata-configuration make-kanata-configuration
  kanata-configuration?
  ;; Package providing the 'kanata' binary. You must set this.
  (package kanata-configuration-package 
           (default (specification->package "kanata")))
  ;; Optional: file-like Kanata config. If provided, it will be installed to /etc/<etc-path>
  ;; and the service will run with '-c /etc/<etc-path>'.
  (config-file kanata-configuration-config-file (default #f))
  ;; Where under /etc to place the config (if config-file is provided).
  ;; Example: "kanata/pocket3.kbd"
  (etc-path kanata-configuration-etc-path (default "kanata/kanata.kbd"))
  ;; Extra command-line arguments (list of strings) appended after the config flag.
  (extra-arguments kanata-configuration-extra-arguments (default '()))
  ;; Log file path used by Shepherd for stdout/stderr of the daemon.
  (log-file kanata-configuration-log-file (default "/var/log/kanata.log"))
  ;; Whether Shepherd should respawn the Kanata process if it exits.
  (respawn? kanata-configuration-respawn? (default #f)))

;; Shepherd services produced by this service type.
(define (kanata-shepherd-services cfg)
  (let* ((etc-path        (kanata-configuration-etc-path cfg))
         (have-config?    (not (not (kanata-configuration-config-file cfg))))
         (extra-args      (kanata-configuration-extra-arguments cfg))
         (log-file        (kanata-configuration-log-file cfg))
         (respawn?        (kanata-configuration-respawn? cfg))
         (kanata-package  (kanata-configuration-package cfg)))
    (list
     ;; One-shot service to load the 'uinput' kernel module before Kanata.
     (shepherd-service
       (provision '(uinput))
       (documentation "Load the 'uinput' kernel module needed by Kanata.")
       (requirement '(file-systems))                       ; mirrors systemd After=local-fs.target
       (one-shot? #t)
       (start #~(lambda _
                  (invoke #$(file-append kmod "/bin/modprobe") "uinput")
                  #t))
       (stop  #~(lambda _ #t)))
     ;; The Kanata daemon itself.
     (shepherd-service
       (provision '(kanata))
       (documentation "Kanata keyboard remapping daemon.")
       (requirement '(file-systems udev uinput))           ; ensure devices & filesystems are up
       (start #~(make-forkexec-constructor
                 (append
                  (list #$(file-append kanata-package "/bin/kanata"))
                  (if #$have-config?
                      (list "-c" (string-append "/etc/" #$etc-path))
                      '())
                  #$extra-args)
                 #:log-file #$log-file))
       (stop #~(make-kill-destructor))
       (respawn? respawn?)))))                              ; controlled by cfg

;; If a config file is provided, install it under /etc/<etc-path>.
(define (kanata-etc-entries cfg)
  (match (kanata-configuration-config-file cfg)
    (#f '())
    (file-like
     (list (cons (kanata-configuration-etc-path cfg) file-like)))))

;; Public service type you can add to your OS ‘services’.
(define kanata-service-type
  (service-type
   (name 'kanata)
   (extensions
    (list
     (service-extension shepherd-root-service-type kanata-shepherd-services)
     (service-extension etc-service-type           kanata-etc-entries)))
   (description
    "Run the Kanata keyboard remapper at boot with Shepherd. Loads the 'uinput'
kernel module first and optionally installs a Kanata config under /etc.")))
