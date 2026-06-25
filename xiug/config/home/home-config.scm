(define-module (xiug config home home-config)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages codex)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang-apps)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages kde-multimedia)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncdu)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages rust)
  #:use-module (nongnu packages productivity)
  #:use-module (saayix packages file-managers)
  #:use-module (saayix packages terminals)
;;   #:use-module (px packages audio)
  #:use-module (xiug config packages fonts font-nerd-fira-code)
  #:use-module (xiug config packages vpn eduvpn)
  #:use-module (xiug config home services impure-symlinks))

(define home-config
  (home-environment
   (packages 
    (append
     (list git

           lsd
           bat
           zoxide
           fzf
           fd
           ripgrep
           yazi

           starship

           unzip

           tree

           ncdu

           brightnessctl
           playerctl

           easyeffects
;;           deepfilternet-ladspa (px packages audio needed for that)

           sqlite

;;           codex

           foot
           ghostty
           wezterm
           qutebrowser

           zotero

           obs

           haruna

           gp-saml-gui
           network-manager-applet

           eduvpn-client

           opensnitch-ui

           hyprlock
           hypridle

           wofi
           dunst
           cliphist

           polkit-kde-agent

           pinentry-qt

           qt6ct

           emacs-doom-themes
           
           emacs-vertico
           emacs-orderless
           emacs-consult
           emacs-embark
           emacs-which-key

           emacs-vertico-posframe

           emacs-olivetti
           
           emacs-magit
           
           emacs-corfu
           emacs-lsp-mode
           emacs-lsp-java
           emacs-cape
           
           emacs-org-download
           
           emacs-geiser
           emacs-geiser-guile
           emacs-guix
           
           emacs-rainbow-delimiters
           emacs-indent-bars

           guile-3.0-latest

           go
           gopls

           rust
           (list rust "tools")
           (list rust "cargo")
           (list rust "rust-src")
           rust-analyzer

           tree-sitter-go
           tree-sitter-gomod
           tree-sitter-gosum
           tree-sitter-java
           tree-sitter-java-properties
           tree-sitter-rust

           font-google-noto-emoji
           font-nerd-fira-code)
     (specifications->packages
      (list "openjdk@21:jdk"))))
   (services
    (append
     (list
      (service home-zsh-service-type)

      (service home-files-service-type
               `((".guile" ,%default-dotguile)
	             ("./.config/nvim" ,
  	              (symlink-to "/home/kivilaak/Dotfiles/nvim"))
	             ("./.config/emacs" ,
  	              (symlink-to "/home/kivilaak/Dotfiles/emacs"))
  	             ("./.config/zsh/.zshrc" ,
  	              (symlink-to "/home/kivilaak/Dotfiles/zsh/zshrc"))
  	             ("./.config/oh-my-posh" ,
  	              (symlink-to "/home/kivilaak/Dotfiles/oh-my-posh"))
  	             ("./.config/tmux" ,
  	              (symlink-to "/home/kivilaak/Dotfiles/tmux"))
  	             ("./.config/hypr/hyprland.conf" ,
  	              (symlink-to "/home/kivilaak/Dotfiles/hyprland/hyprland.conf"))
  	             ("./.config/hypr/hyprland" ,
  	              (symlink-to "/home/kivilaak/Dotfiles/hyprland/hyprland"))
  	             ("./.config/hypr/hyprlock.conf" ,
  	              (symlink-to "/home/kivilaak/Dotfiles/hyprlock/hyprlock.conf"))
                 ("./.config/wezterm" ,
                  (symlink-to "/home/kivilaak/Dotfiles/wezterm"))
                 ("./.config/nushell" ,
                  (symlink-to "/home/kivilaak/Dotfiles/nushell"))
                 (".Xdefaults" ,%default-xdefaults)))

      (service home-xdg-configuration-files-service-type
               `(("gdb/gdbinit" ,%default-gdbinit)
                 ("nano/nanorc" ,%default-nanorc)))
	  (impure-symlinks-service
	   #:symlinks
	   '((".ssh"      . "/villa/kivilaak/.ssh")
	     ("Code"      . "/villa/kivilaak/Code")
	     ("Documents" . "/villa/kivilaak/Documents")
	     ("Dotfiles"  . "/villa/kivilaak/Dotfiles")
	     ("Downloads" . "/villa/kivilaak/Downloads")
	     ("Videos"    . "/villa/kivilaak/Videos")))

      (simple-service 'additional-home-environment-variables
		              home-environment-variables-service-type
                      `(("EMACSLOADPATH"
                         . ,(string-append
                             (or (getenv "EMACSLOADPATH") "")
                             (if (getenv "EMACSLOADPATH") ":" "")
                             (getenv "HOME") "/.guix-home/profile/share/emacs/site-lisp"))
                        ("XDG_DATA_DIRS"
                         . ,(string-append
                             "/var/lib/flatpak/exports/share:"
                             (getenv "HOME") "/.local/share/flatpak/exports/share"
                             (if (getenv "XDG_DATA_DIRS") ":" "")
                             (or (getenv "XDG_DATA_DIRS") "")))))


	  (service home-dbus-service-type)
	  (service home-pipewire-service-type))

     %base-home-services))))

home-config
