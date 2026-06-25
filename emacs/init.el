(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-always-indent 'complete)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(blink-cursor-mode 0)

(setq scroll-margin 6)
(setq scroll-step 1)
(setq scroll-conservatively 101)
(pixel-scroll-precision-mode 1)
(setq mouse-wheel-progressive-speed nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(electric-pair-mode 1)
(electric-indent-mode 1)

(global-auto-revert-mode 1)
(setq auto-revert-avoid-polling t)
(setq global-auto-revert-non-file-buffers t)

(save-place-mode 1)

(recentf-mode 1)
(setq recentf-max-saved-items 200)

(global-subword-mode 1)

(setq sentence-end-double-space nil)

(delete-selection-mode 1)

(let* ((cache (or (getenv "XDG_CACHE_HOME")
                  (expand-file-name "~/.cache")))
       (emacs-cache (expand-file-name "emacs/" cache))
       (backups (expand-file-name "backups/" emacs-cache))
       (autosaves (expand-file-name "auto-saves/" emacs-cache)))
  (make-directory backups t)
  (make-directory autosaves t)

  (setq backup-directory-alist `(("." . ,backups)))
  (setq auto-save-file-name-transforms `((".*" ,autosaves t)))
  (setq auto-save-list-file-prefix (expand-file-name ".saves-" autosaves)))

(load-theme 'doom-horizon t)

(require 'vertico)
(vertico-mode 1)
(setq vertico-cycle t)
(setq vertico-resize t)

(require 'vertico-directory)
(define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
(define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
(define-key vertico-map (kbd "C-w") #'vertico-directory-delete-word)

(require 'vertico-posframe)
(vertico-posframe-mode 1)

(setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)

(setq vertico-posframe-width 60)
(setq vertico-posframe-height 12)
(setq vertico-posframe-min-width 40)
(setq vertico-posframe-min-height 9)

(setq vertico-posframe-parameters
      '((left-fringe  . 8)
        (right-fringe . 8)))

(setq vertico-posframe-border-width 2)

(with-eval-after-load 'vertico-posframe
  (set-face-attribute 'child-frame-border nil
                      :background "#d0d0d0")
  (set-face-attribute 'vertico-posframe nil
                      :background "#141414"))

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides
      '((file (styles partial-completion))))

(require 'consult)
(global-set-key (kbd "C-x b") #'consult-buffer)
(global-set-key (kbd "C-s") #'consult-line)
(global-set-key (kbd "C-c s") #'consult-ripgrep)
(global-set-key (kbd "C-x C-r") #'consult-recent-file)
(global-set-key (kbd "C-c i") #'consult-imenu)
(global-set-key (kbd "C-c SPC") #'consult-mark)
(global-set-key (kbd "C-c f f") #'consult-find)
(global-set-key (kbd "C-c f l") #'consult-locate)

(require 'which-key)
(which-key-mode 1)
(setq which-key-idle-delay 0.35)
(setq which-key-idle-secondary-delay 0.05)

(require 'embark)
(global-set-key (kbd "C-.") #'embark-act)
(global-set-key (kbd "C-;") #'embark-dwim)
(setq embark-verbose-indicator-display-action '(display-buffer-at-bottom))
(setq embark-action-indicator
      (lambda (map)
        (which-key--show-keymap "Embark" map nil nil 'no-paging)))

(require 'magit)
(global-set-key (kbd "C-x g") #'magit-status)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

;; TODO - corfu for terminal
(require 'corfu)
(global-corfu-mode 1)
(setq corfu-auto t)
(setq corfu-auto-delay 0.05)
(setq corfu-auto-prefix 2)
(setq corfu-cycle t)
(setq corfu-preselect 'prompt)
(corfu-popupinfo-mode 1)
(setq corfu-popupinfo-delay '(0.5 . 0.2))
(define-key corfu-map (kbd "C-g") #'corfu-quit)
(define-key corfu-map (kbd "RET") nil)
(define-key corfu-map (kbd "<return>") nil)
(define-key corfu-map (kbd "C-j") #'corfu-insert)
(global-set-key (kbd "M-TAB") #'completion-at-point)

(require 'subr-x)
(defun my-guix-ts-dirs ()
  "Return possible Guix tree-sitter grammar directories."
  (let* ((profiles (list
                    (getenv "GUIX_PROFILE")
                    (expand-file-name "~/.guix-profile")
                    (expand-file-name "~/.guix-home/profile")
                    "/run/current-system/profile"))
         (dirs (mapcar (lambda (p)
                         (when (and p (stringp p))
                           (expand-file-name "lib/tree-sitter" p)))
                       profiles)))
    (seq-filter (lambda (d) (and (stringp d) (file-directory-p d)))
                dirs)))

(setq treesit-extra-load-path
      (delete-dups (append treesit-extra-load-path (my-guix-ts-dirs))))

(when (fboundp 'go-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode)))

(when (fboundp 'go-mod-ts-mode)
  (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode)))

(when (fboundp 'go-sum-ts-mode)
  (add-to-list 'auto-mode-alist '("/go\\.sum\\'" . go-sum-ts-mode)))

(add-hook 'go-ts-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode t
                        tab-width 4)))

(require 'project)
(require 'cl-lib)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

(require 'eglot)

(defun my-go-eglot-save-hooks ()
  (add-hook 'before-save-hook
            (lambda ()
              (when (eglot-managed-p)
                (eglot-code-action-organize-imports)))
            -20 t)
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-ts-mode-hook #'eglot-ensure)
(add-hook 'go-ts-mode-hook #'my-go-eglot-save-hooks)

(require 'cape)
(setq cape-dabbrev-check-other-buffers 'some)
(add-to-list 'completion-at-point-functions #'cape-file t)
(add-to-list 'completion-at-point-functions #'cape-dabbrev t)
(add-to-list 'completion-at-point-functions #'cape-keyword t)

(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'indent-bars)

(add-hook 'prog-mode-hook #'indent-bars-mode)

(setq indent-bars-color '(highlight :face-bg t :blend 0.15))
(setq indent-bars-pattern ".")
(setq indent-bars-width-frac 0.05)
(setq indent-bars-pad-frac 0.1)

(require 'geiser-guile) ; or geiser-racket, etc. — match your Scheme

(setq geiser-default-implementation 'guile)
(setq geiser-active-implementations '(guile))

;; Auto-start a REPL when visiting Scheme files, per project
(setq geiser-repl-per-project-p t)

;; Start Geiser mode automatically in scheme buffers
(add-hook 'scheme-mode-hook #'geiser-mode)

;; Optionally auto-start the REPL on first scheme file visit
(setq geiser-mode-start-repl-p t)

(require 'org)

(setq org-directory (expand-file-name "~/Documents/vault/"))
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
(setq org-agenda-files
      (list
       (expand-file-name "inbox.org" org-directory)
       (expand-file-name "agenda/" org-directory)))

(global-set-key (kbd "C-c a") #'org-agenda)
(setq org-log-done 'time)
(setq org-startup-indented t)
(setq org-hide-emphasis-markers t)

(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive)

(require 'org-attach)

(defun my/note-assets-dir ()
  "Return a note-local assets directory for the current file."
  (unless (buffer-file-name)
    (user-error "Save file before attaching files"))
  (concat (file-name-sans-extension (buffer-file-name)) ".assets/"))

(setq org-attach-method 'cp
      org-attach-store-link-p t
      org-attach-use-inheritance nil)

(add-hook 'org-mode-hook
          (lambda ()
            (when (buffer-file-name)
              (setq-local org-attach-id-dir (my/note-assets-dir)))))

(require 'org-download)

(setq org-download-method 'attach)
(setq org-download-heading-lvl nil)

(add-hook 'dired-mode-hook #'org-download-enable)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c I") #'org-download-clipboard))

(set-face-attribute 'default nil
                    :font "FiraCode Nerd Font Mono"
                    :height 120)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
     "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7"
     "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874"
     "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9"
     "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0"
     "9d5124bef86c2348d7d4774ca384ae7b6027ff7f6eb3c401378e298ce605f83a"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     default))
 '(safe-local-variable-values
   '((eval modify-syntax-entry 43 "'") (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (eval progn (require 'lisp-mode)
           (defun emacs27-lisp-fill-paragraph (&optional justify)
             (interactive "P")
             (or (fill-comment-paragraph justify)
                 (let
                     ((paragraph-start
                       (concat paragraph-start
                               "\\|\\s-*\\([(;\"]\\|\\s-:\\|`(\\|#'(\\)"))
                      (paragraph-separate
                       (concat paragraph-separate
                               "\\|\\s-*\".*[,\\.]$"))
                      (fill-column
                       (if
                           (and
                            (integerp emacs-lisp-docstring-fill-column)
                            (derived-mode-p 'emacs-lisp-mode))
                           emacs-lisp-docstring-fill-column
                         fill-column)))
                   (fill-paragraph justify))
                 t))
           (setq-local fill-paragraph-function
                       #'emacs27-lisp-fill-paragraph))
     (geiser-insert-actual-lambda) (geiser-repl-per-project-p . t)
     (eval with-eval-after-load 'yasnippet
           (let
               ((guix-yasnippets
                 (expand-file-name "etc/snippets/yas"
                                   (locate-dominating-file
                                    default-directory ".dir-locals.el"))))
             (unless (member guix-yasnippets yas-snippet-dirs)
               (add-to-list 'yas-snippet-dirs guix-yasnippets)
               (yas-reload-all))))
     (eval with-eval-after-load 'tempel
           (if (stringp tempel-path)
               (setq tempel-path (list tempel-path)))
           (let
               ((guix-tempel-snippets
                 (concat
                  (expand-file-name "etc/snippets/tempel"
                                    (locate-dominating-file
                                     default-directory
                                     ".dir-locals.el"))
                  "/*.eld")))
             (unless (member guix-tempel-snippets tempel-path)
               (add-to-list 'tempel-path guix-tempel-snippets))))
     (eval with-eval-after-load 'git-commit
           (add-to-list 'git-commit-trailers "Change-Id"))
     (eval setq-local guix-directory
           (locate-dominating-file default-directory ".dir-locals.el"))
     (eval add-to-list 'completion-ignored-extensions ".go"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
