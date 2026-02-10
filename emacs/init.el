(setq-default indent-tabs-mode nil)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(blink-cursor-mode 0)

(setq scroll-margin 5)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)
(pixel-scroll-precision-mode 1)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(require 'vertico)
(vertico-mode 1)
(setq vertico-cycle t)
(setq vertico-resize t)

(require 'vertico-directory)
(define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
(define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
(define-key vertico-map (kbd "C-w") #'vertico-directory-delete-word)

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
(setq corfu-auto-delay 0.15)
(setq corfu-auto-prefix 2)
(setq corfu-cycle t)
(setq corfu-preselect 'prompt)
(corfu-popupinfo-mode 1)
(setq corfu-popupinfo-delay '(0.5 . 0.2))
(define-key corfu-map (kbd "C-g") #'corfu-quit)
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
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-keyword)

(require 'org)

(setq org-directory (expand-file-name "~/Documents/vault/"))
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
(setq org-agenda-files
      (list
       (expand-file-name "agenda/" org-directory)))

(global-set-key (kbd "C-c a") #'org-agenda)
(setq org-log-done 'time)
(setq org-startup-indented t)
(setq org-hide-emphasis-markers t)

(require 'org-roam)

(setq org-roam-directory
      (file-truename (expand-file-name "roam/" org-directory)))

(setq org-roam-db-location
      (expand-file-name "org-roam.db" org-roam-directory))

(unless (file-directory-p org-roam-directory)
  (make-directory org-roam-directory t))

(org-roam-db-autosync-mode 1)

(global-set-key (kbd "C-c n f") #'org-roam-node-find)
(global-set-key (kbd "C-c n i") #'org-roam-node-insert)
(global-set-key (kbd "C-c n c") #'org-roam-capture)
(global-set-key (kbd "C-c n l") #'org-roam-buffer-toggle)

(require 'org-download)

(setq org-download-method 'directory)
(setq org-download-image-dir "./images")
(setq org-download-heading-lvl nil)

(add-hook 'dired-mode-hook #'org-download-enable)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c I") #'org-download-clipboard))

(load-theme 'doom-outrun-electric t)

(set-face-attribute 'default nil
                    :font "FiraCode Nerd Font Mono"
                    :height 120)

