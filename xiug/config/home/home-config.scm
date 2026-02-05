(define-module (xiug config home home-config)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu services)
  #:use-module (gnu services desktop)
  #:use-module (guix gexp)
  #:use-module (gnu system shadow))

(define home-config
  (home-environment
    (services
      (append
        (list
          (service home-zsh-service-type)

          (service home-files-service-type
           `((".guile" ,%default-dotguile)
	     ("./.config/nvim" ,
	      (symlink-to "/home/kivilaak/Dotfiles/nvim"))
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

	  (service home-dotfiles-service-type 
		   (home-dotfiles-configuration
		     (excluded '("^$"))
		     (directories '("/villa/kivilaak"))))

	  (service home-dbus-service-type)
	  (service home-pipewire-service-type)
      (service upower-service-type))

        %base-home-services))))

home-config
