(define-module (packages fonts fira-code-nerd)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system font)
  #:use-module ((guix licenses) #:prefix license:))

(define version "3.4.0")

(define-public font-nerd-fira-code
  (package
    (name "font-nerd-fira-code")
    (version version)
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
                    version "/FiraCode.tar.xz"))
              (sha256
               (base32 "0zg4ng2k07zwk7q3rdd3bvxa915qrsk8d67isqf56np0w29v0gyq"))))
    (build-system font-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (let ((target (string-append (getcwd) "/src")))
                (mkdir target)
                (invoke "tar" "xf" #$source "-C" target)))))))
    (home-page "https://www.nerdfonts.com")
    (synopsis "Fira Code patched with Nerd Font glyphs")
    (description "This package provides the Fira Code mono font family \
patched with thousands of popular developer glyphs using the Nerd Fonts \
project.  It includes regular Fira Code, bold/italic variants and the \
Fura Mono flavour without ligatures.")
    (license license:silofl1.1)))
