(define-module (xiug config packages vpn eduvpn)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz))

;;;
;;; Shared source for the eduvpn-common library + Python wrapper.
;;;
;;; The release tarball contains the Go source tree (with vendor/) and
;;; the Python wrapper under wrappers/python/.
;;;

(define eduvpn-common-version "5.0.1")

(define eduvpn-common-source
  (origin
   (method url-fetch)
   (uri (string-append
         "https://codeberg.org/eduVPN/eduvpn-common/releases/download/"
         eduvpn-common-version
         "/eduvpn-common-" eduvpn-common-version ".tar.xz"))
   (sha256
    (base32
     "1hqjr37hq3ganwfyxy6q87jns5ibik3w2lq8nczfwmj4y6mgx5sv"))))


;;; -------------------------------------------------------------------
;;; 1. libeduvpn-common — Go → C shared library
;;; -------------------------------------------------------------------

(define-public libeduvpn-common
  (package
   (name "libeduvpn-common")
   (version eduvpn-common-version)
   (source eduvpn-common-source)
   (build-system gnu-build-system)
   (arguments
    (list
     #:tests? #f                       ; no meaningful test suite for a
                                        ; c-shared build target
     #:phases
     #~(modify-phases %standard-phases
                      ;; The tarball has no configure script.
                      (delete 'configure)

                      (replace 'build
                               (lambda _
                                 ;; The release tarball ships vendor/, so -mod=vendor avoids
                                 ;; any network access.
                                 ;; -buildmode=c-shared: produce .so + .h
                                 ;; -tags=release:       upstream stable-API gate
                                 ;; -trimpath:           reproducibility (strip builder paths)
                                 (setenv "GOPATH" "/tmp/go")
                                 (setenv "GOCACHE" "/tmp/go-cache")
                                 (invoke "go" "build"
                                         "-v"
                                         "-trimpath"
                                         "-mod=vendor"
                                         "-buildmode=c-shared"
                                         "-tags=release"
                                         "-o" (string-append "libeduvpn_common-"
                                                             #$version ".so")
                                         "./exports")))

                      (replace 'install
                               (lambda* (#:key outputs #:allow-other-keys)
                                 (let* ((out  (assoc-ref outputs "out"))
                                        (lib  (string-append out "/lib"))
                                        (inc  (string-append out "/include"))
                                        (base (string-append "libeduvpn_common-" #$version)))
                                   ;; Shared library.
                                   (install-file (string-append base ".so") lib)
                                   ;; C header emitted by -buildmode=c-shared.
                                   ;; Not needed by the Python wrapper, but useful for
                                   ;; hypothetical C consumers and costs nothing.
                                   (when (file-exists? (string-append base ".h"))
                                     (install-file (string-append base ".h") inc))))))))
   (native-inputs
    (list go))
   (home-page "https://codeberg.org/eduVPN/eduvpn-common")
   (synopsis "Shared eduVPN client logic as a C shared library")
   (description
    "The @code{eduvpn-common} library contains code shared among all eduVPN
client implementations.  It is compiled from Go into a C-compatible shared
library (@file{libeduvpn_common-VERSION.so}) exposing an opaque handle-based
API for VPN session management, server discovery, and OAuth 2.0 flows.")
   (license license:expat)))


;;; -------------------------------------------------------------------
;;; 2. python-eduvpn-common — Python ctypes wrapper
;;; -------------------------------------------------------------------

(define-public python-eduvpn-common
  (package
   (name "python-eduvpn-common")
   (version eduvpn-common-version)
   (source eduvpn-common-source)
   (build-system pyproject-build-system)
   (arguments
    (list
     #:tests? #f                       ; no standalone test suite in
                                        ; wrappers/python/
     #:phases
     #~(modify-phases %standard-phases
                      (add-after 'unpack 'stage-shared-library-and-chdir
                                 (lambda* (#:key inputs #:allow-other-keys)
                                   ;; Upstream's wheel build expects the .so inside the Python
                                   ;; package tree so it becomes package-data in the installed
                                   ;; egg/wheel.
                                   ;;
                                   ;; IMPORTANT: verify whether upstream places the .so directly
                                   ;; in eduvpn_common/ or in eduvpn_common/lib/.  The answer
                                   ;; depends on the ctypes loader in __init__.py.  Adjust the
                                   ;; dst-dir below if needed.  For 5.x, eduvpn_common/lib/
                                   ;; appears to be the convention (matching candidate 2 and the
                                   ;; upstream Makefile 'pack' target).
                                   (let* ((lib-out (assoc-ref inputs "libeduvpn-common"))
                                          (soname  (string-append "libeduvpn_common-"
                                                                  #$version ".so"))
                                          (dst-dir "wrappers/python/eduvpn_common/lib/"))
                                     (mkdir-p dst-dir)
                                     (copy-file (string-append lib-out "/lib/" soname)
                                                (string-append dst-dir soname)))
                                   ;; Descend so all subsequent phases see pyproject.toml.
                                   (chdir "wrappers/python"))))))
   ;; Build-time: copy the .so from this input.
   (inputs
    (list libeduvpn-common))
   ;; Run-time: if the ctypes loader ever falls back to dlopen() by bare
   ;; name, propagating ensures the .so is on LD_LIBRARY_PATH in profiles.
   (propagated-inputs
    (list libeduvpn-common))
   (native-inputs
    (list python-setuptools
          python-wheel))
   (home-page "https://codeberg.org/eduVPN/eduvpn-common")
   (synopsis "Python bindings for the eduVPN common library")
   (description
    "The @code{eduvpn_common} Python package is a ctypes-based wrapper
around @code{libeduvpn-common}.  It provides the Python API for VPN session
management, server discovery, and OAuth 2.0 flows used by the eduVPN Linux
client.")
   (license license:expat)))


;;; -------------------------------------------------------------------
;;; 3. eduvpn-client — GTK 3 GUI + CLI application
;;; -------------------------------------------------------------------

(define-public eduvpn-client
  (package
   (name "eduvpn-client")
   (version "4.7.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://codeberg.org/eduVPN/linux-app/releases/download/"
           version "/linux-app-" version ".tar.xz"))
     (sha256
      (base32
       "145i8apcm4lma0k4dbd4ad367zaahy25szl0i1djjqfyq0y9y04j"))))
   (build-system pyproject-build-system)
   (arguments
    (list
     ;; Tests require a running D-Bus session, NetworkManager, and a
     ;; display server.  Disable until a sandboxed test harness is set up.
     #:tests? #f

     #:phases
     #~(modify-phases %standard-phases
                      ;; Expose desktop files, icons, GSettings schemas, and D-Bus
                      ;; service files that upstream installs under
                      ;; site-packages/eduvpn/data/share/ to the top-level $out/share
                      ;; so that XDG tooling can find them.
                      (add-after 'install 'expose-share-directory
                                 (lambda* (#:key outputs #:allow-other-keys)
                                   (let* ((out     (assoc-ref outputs "out"))
                                          ;; find-files is always in scope from (guix build utils).
                                          ;; Locate the first "share" directory anywhere under $out/lib,
                                          ;; which avoids hard-coding the Python version string entirely.
                                          (matches (find-files (string-append out "/lib")
                                                               "^share$"
                                                               #:directories? #t)))
                                     (unless (null? matches)
                                       (symlink (car matches)
                                                (string-append out "/share"))))))

                      (add-after 'expose-share-directory 'compile-schemas
                                 (lambda* (#:key outputs #:allow-other-keys)
                                   (let ((schemas (string-append (assoc-ref outputs "out")
                                                                 "/share/glib-2.0/schemas")))
                                     (when (directory-exists? schemas)
                                       (invoke "glib-compile-schemas" schemas)))))

                      (add-after 'wrap 'wrap-gi
                                 (lambda* (#:key inputs outputs #:allow-other-keys)
                                   (let* ((out (assoc-ref outputs "out"))
                                          (gi-typelib-path
                                           (string-join
                                            (filter file-exists?
                                                    (map (lambda (p)
                                                           (string-append (cdr p)
                                                                          "/lib/girepository-1.0"))
                                                         inputs))
                                            ":")))
                                     (for-each
                                      (lambda (prog)
                                        (wrap-program prog
                                                      `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
                                      (find-files (string-append out "/bin") "^[^.]"))))))))

   (native-inputs
    (list gobject-introspection
          ;; glib:bin provides glib-compile-schemas and
          ;; glib-compile-resources.
          `(,glib "bin")
          python-setuptools
          python-wheel))

   (inputs
    (list gdk-pixbuf
          gtk+
          hicolor-icon-theme
          libnotify
          libsecret
          network-manager
          python-eduvpn-common
          python-pygobject))

   (home-page "https://codeberg.org/eduVPN/linux-app")
   (synopsis "Linux desktop client for eduVPN")
   (description
    "The eduVPN client is the GNU/Linux desktop application for eduVPN.
It provides @command{eduvpn-gui}, a GTK 3 graphical interface, and
@command{eduvpn-cli}, a command-line interface, for connecting to eduVPN and
Let's Connect! servers.

At run time, NetworkManager must be active and reachable over D-Bus.
Depending on the VPN protocol, @code{network-manager-openvpn} (for OpenVPN)
or WireGuard kernel support may also be required.")
   (license license:gpl3+)))
