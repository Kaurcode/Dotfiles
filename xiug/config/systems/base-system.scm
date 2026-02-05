(define-module (xiug config systems base-system)
               #:use-module (gnu)
               #:use-module (gnu system nss)
               #:use-module (guix utils)
               #:use-module (srfi srfi-1)
               #:use-module (nongnu packages linux)
               #:use-module (nongnu system linux-initrd)
               #:use-module (xiug config services kanata)
               #:use-module (xiug config bootloader grub)
               #:export (base-system))

(use-package-modules
  certs
  vim
  admin
  gnome
  wm
  shells
  tmux
  linux
  freedesktop
  kde-plasma
  package-management
  terminals
  web-browsers
  version-control
  xdisorg
  ssh
  rust-apps
  commencement
  qt
  emacs
  emacs-xyz
  polkit
  fonts
  disk
  video)

(use-service-modules 
  desktop
  networking
  sddm
  dbus
  pm)

(define %vault
  (mapped-device 
    (source (uuid "7de76f0a-a08e-4e32-9218-60fe80709cc9"))
    (target "vault")
    (type luks-device-mapping)))


(define %corevg
  (mapped-device
    (source "CoreVG")
    (targets (list "CoreVG-swap"
                   "CoreVG-guixroot"
                   "CoreVG-guixvar"
                   "CoreVG-opt"
                   "CoreVG-guixhome"
                   "CoreVG-flatpak"
                   "CoreVG-villa"
                   "CoreVG-gxstorevar"))
    (type lvm-device-mapping)))

(define %aethervg
  (mapped-device
    (source "AetherVG")
    (targets (list "AetherVG-guixstore"))
    (type lvm-device-mapping)))

(define-public base-system
  (operating-system
    (services (append (list 
                        (service elogind-service-type)
                        (service network-manager-service-type
                                 (network-manager-configuration
                                   (vpn-plugins (list network-manager-openconnect))))
                        (service wpa-supplicant-service-type)
                        (service nftables-service-type)
                        (service dbus-root-service-type)
                        (service sddm-service-type
                                 (sddm-configuration
                                   (display-server wayland)))
                        (service plasma-desktop-service-type)
                        (service upower-service-type)
                        (service power-profiles-daemon-service-type)
                        (service udisks-service-type)
                        (service gvfs-service-type)
                        (service polkit-service-type)
                        polkit-wheel-service
                        (service kanata-service-type
                                 (kanata-configuration 
                                   (config-file (local-file "../../../kanata/kanata.kbd")))))
                      (modify-services %base-services
                                       (guix-service-type config => (guix-configuration
                                                                      (inherit config)
                                                                      (substitute-urls
                                                                        (append (list 
                                                                                  "https://substitutes.nonguix.org"
                                                                                  "https://cache-cdn.guix.moe")
                                                                                %default-substitute-urls))
                                                                      (authorized-keys
                                                                        (append (list (plain-file "non-guix.pub"
  "(public-key
    (ecc
      (curve Ed25519)
      (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
      )
    )"
                                                                                                  )
                                                                                      (plain-file "guix-moe-old.pub"
                                                                                                  "(public-key (ecc (curve Ed25519) (q #374EC58F5F2EC0412431723AF2D527AD626B049D657B5633AAAEBC694F3E33F9#)))")
                                                                                      (plain-file "guix-moe.pub"
                                                                                                  "(public-key (ecc (curve Ed25519) (q #552F670D5005D7EB6ACF05284A1066E52156B51D75DE3EBD3030CD046675D543#)))"))
                                                                        %default-authorized-guix-keys)))))))
  
    (kernel linux)
    (initrd microcode-initrd)
    (firmware (list linux-firmware))
    (host-name "artemis")
    (timezone "Europe/Tallinn")
    (locale "en_US.utf8")
  
    (keyboard-layout (keyboard-layout "ee"))
  
    (bootloader (bootloader-configuration
                  (bootloader grub-efi-lvm-no-crypto-bootloader)
                  (targets '("/efi"))
                  (keyboard-layout keyboard-layout)))
  
    (mapped-devices
      (list %vault %corevg %aethervg))
  
    (file-systems (append
                    (list (file-system
                            (device (uuid "DC85-02B8" 'fat))
                            (mount-point "/efi")
                            (type "vfat")
                            (needed-for-boot? #t))
                          (file-system
                            (device (uuid "dd3cb5be-0449-4808-ae72-dddc980cba99"))
                            (mount-point "/boot")
                            (type "ext4")
                            (needed-for-boot? #t))
                          (file-system
                            (device "/dev/mapper/CoreVG-guixroot")
                            (mount-point "/")
                            (type "btrfs")
                            (dependencies (list %vault %corevg)))
                          (file-system
                            (device "/dev/mapper/CoreVG-guixvar")
                            (mount-point "/var")
                            (type "ext4")
                            (needed-for-boot? #t)
                            (dependencies (list %vault %corevg)))
                          (file-system
                            (device "/dev/mapper/CoreVG-opt")
                            (mount-point "/opt/ultra")
                            (type "ext4")
                            (dependencies (list %vault %corevg)))
                          (file-system
                            (device "/dev/mapper/CoreVG-guixhome")
                            (mount-point "/home")
                            (type "btrfs")
                            (dependencies (list %vault %corevg)))
                          (file-system
                            (device "/dev/mapper/CoreVG-villa")
                            (mount-point "/villa")
                            (type "btrfs")
                            (dependencies (list %vault %corevg)))
                          (file-system
                            (device "/dev/mapper/CoreVG-flatpak")
                            (mount-point "/var/lib/flatpak")
                            (type "btrfs")
                            (dependencies (list %vault %corevg)))
                          (file-system
                            (device "/dev/mapper/CoreVG-gxstorevar")
                            (mount-point "/var/guix")
                            (type "ext4")
                            (needed-for-boot? #t)
                            (dependencies (list %vault %corevg)))
                          (file-system
                            (device "/dev/mapper/AetherVG-guixstore")
                            (mount-point "/gnu")
                            (type "btrfs")
                            (needed-for-boot? #t)
                            (flags '(no-atime))
                            (options "compress=zstd:3,space_cache=v2,ssd")
                            (dependencies (list %aethervg))))
                    %base-file-systems))
  
    (swap-devices (list (swap-space
                          (target "/dev/mapper/CoreVG-swap")
                          (dependencies (list %vault %corevg)))))
  
    (users (cons (user-account
                   (name "kivilaak")
                   (group "kivilaak")
                   (uid 1001)
                   (supplementary-groups '("wheel" "netdev"
                                           "audio" "video"
                                           "jetbrains"))
                   (shell (file-append zsh "/bin/zsh")))
                 %base-user-accounts))
  
    (groups (cons* (user-group
                     (name "kivilaak")
                     (id 1001))
                   (user-group
                     (name "jetbrains")
                     (id 1002))
                   %base-groups))
  
    (sudoers-file
      (mixed-text-file "sudoers"
                       (plain-file-content %sudoers-specification)
                       "\nDefaults editor=/run/current-system/profile/bin/nvim, !env_editor"))
  
    (packages (append (list
                        neovim
                        wpa-supplicant
                        network-manager
                        hyprland
                        zsh
                        tmux
                        dunst
                        pipewire
                        wireplumber
                        xdg-desktop-portal-hyprland
                        xdg-desktop-portal-kde
                        xdg-desktop-portal-wlr
                        xdg-desktop-portal-gtk
                        xdg-desktop-portal
                        flatpak
                        foot
                        qutebrowser
                        git
          		      wofi
          		      openssh
          		      lsd
          		      bat
          		      zoxide
          		      fzf
          		      qt6ct
          		      qtwayland
          		      gcc-toolchain
          		      ripgrep
          		      emacs-pgtk
          		      emacs-evil
          		      emacs-evil-collection
          		      emacs-evil-surround
          		      emacs-evil-commentary
          		      emacs-evil-leader
          		      emacs-which-key
          		      emacs-ivy
          		      emacs-magit
          		      emacs-consult
          		      emacs-orderless
          		      emacs-marginalia
          		      emacs-yasnippet
          		      wl-clipboard
          		      polkit
          		      hyprpolkitagent
          		      font-google-noto-emoji
          		      hyprlock
          		      hypridle
          		      lvm2
          		      btrfs-progs
          		      e2fsprogs
          		      dosfstools
          		      mpvpaper)
                      %base-packages))
  
    (name-service-switch %mdns-host-lookup-nss))
)
