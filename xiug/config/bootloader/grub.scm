(define-module (xiug config bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:export (grub-efi-lvm-no-crypto-bootloader))

(define make-grub-configuration
  (@@ (gnu bootloader grub) make-grub-configuration))

(define* (grub-lvm-no-crypto-configuration-file config entries
                                            #:key
                                            (locale #f)
                                            (system (%current-system))
                                            (old-entries '())
                                            (store-crypto-devices '())
                                            store-directory-prefix
                                            #:allow-other-keys)
         (let* ((bootloader (bootloader-configuration-bootloader config))
                (grub       (bootloader-package bootloader))
                (base-cfg
                  (make-grub-configuration grub config entries
                                           #:locale locale
                                           #:system system
                                           #:old-entries old-entries
                                           #:store-crypto-devices '()
                                           #:store-directory-prefix store-directory-prefix)))
           (computed-file "grub.cfg"
                          #~(begin
                              (use-modules (rnrs io ports))
                              (call-with-input-file #$base-cfg
                                                    (lambda (in)
                                                      (call-with-output-file #$output
                                                                             (lambda (out)
                                                                               (display "insmod lvm\n" out)
                                                                               (let loop ((chunk (get-bytevector-n in 65536)))
                                                                                 (unless (eof-object? chunk)
                                                                                   (put-bytevector out chunk)
                                                                                   (loop (get-bytevector-n in 65536))))))))))))

(define grub-efi-lvm-no-crypto-bootloader
  (bootloader
    (inherit grub-efi-bootloader)
    (name 'grub-efi)
    (configuration-file-generator grub-lvm-no-crypto-configuration-file)))
