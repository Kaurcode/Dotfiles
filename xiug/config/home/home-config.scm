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
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang-apps)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tree-sitter)
  #:use-module (saayix packages file-managers)
  #:use-module (saayix packages terminals)
  #:use-module (px packages vm)
  #:use-module (px packages tools)
  #:use-module (xiug config packages fonts font-nerd-fira-code)
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
              ripgrep
              yazi

              tree

              sqlite

              codex

              foot
              ghostty
              qutebrowser

              gp-saml-gui
              network-manager-applet

              hyprlock
              hypridle

              wofi
              dunst
              ironbar

              polkit-kde-agent

              qt6ct

              emacs-vertico
              emacs-orderless
              emacs-doom-themes
              emacs-consult
              emacs-embark
              emacs-which-key
              emacs-magit
              emacs-corfu
              emacs-lsp-mode
              emacs-lsp-java
              emacs-cape
              emacs-org-roam
              emacs-org-download

              go
              gopls

              tree-sitter-go
              tree-sitter-gomod
              tree-sitter-gosum
              tree-sitter-java
              tree-sitter-java-properties

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
