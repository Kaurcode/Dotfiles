(define-module (xiug config channels channels)
               #:use-module (guix channels))

(cons* (channel
         (name 'nonguix)
         (url "https://gitlab.com/nonguix/nonguix")
         ;; Enable signature verification:
         (introduction
           (make-channel-introduction
             "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
             (openpgp-fingerprint
               "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
;;       (channel
;;         (name 'rosenthal)
;;         (url "https://codeberg.org/hako/rosenthal.git")
;;         (branch "trunk")
;;         (introduction
;;           (make-channel-introduction
;;             "7677db76330121a901604dfbad19077893865f35"
;;             (openpgp-fingerprint
;;               "13E7 6CD6 E649 C28C 3385  4DF5 5E5A A665 6149 17F7"))))
       (channel
         (name 'saayix)
         (branch "main")
         (url "https://codeberg.org/look/saayix")
         (introduction
           (make-channel-introduction
             "12540f593092e9a177eb8a974a57bb4892327752"
             (openpgp-fingerprint
               "3FFA 7335 973E 0A49 47FC  0A8C 38D5 96BE 07D3 34AB"))))
      (channel
        (name 'pantherx)
        (url "https://codeberg.org/gofranz/panther.git")
        (branch "master")
        (introduction
          (make-channel-introduction
            "54b4056ac571611892c743b65f4c47dc298c49da"
            (openpgp-fingerprint
              "A36A D41E ECC7 A871 1003  5D24 524F EB1A 9D33 C9CB"))))
       %default-channels)
