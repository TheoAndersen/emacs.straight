(switch-to-buffer "*straight-process*")
;; STRAIGHT package manager https://github.com/raxod502/straight.el#features
;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Load use-package with straight
;;
(straight-use-package 'use-package)

(progn ; startup
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  ;; (setq package-enable-at-startup nil)
  ;;
  (setq inhibit-startup-buffer-menu t
        inhibit-startup-screen t
        inhibit-startup-echo-area-message nil
        initial-buffer-choice t
        initial-scratch-message
        ";; ▄▄▄ .• ▌ ▄ ·.  ▄▄▄·  ▄▄· .▄▄ ·\n;; ▀▄.▀··██ ▐███▪▐█ ▀█ ▐█ ▌▪▐█ ▀.\n;; ▐▀▀▪▄▐█ ▌▐▌▐█·▄█▀▀█ ██ ▄▄▄▀▀▀█▄\n;; ▐█▄▄▌██ ██▌▐█▌▐█ ▪▐▌▐███▌▐█▄▪▐█\n\n;; “Emacs is like a laser guided missile.\n;;  It only has to be slightly mis-configured\n;;  to ruin your whole day.”  -Sean McGrathi\n\n"
;;        ";; - 'Tis but a scratch!\n;; - A scratch? Your arm's off!\n;; - No, it isn't!\n\n"
        load-prefer-newer t
        ;; disable files from being created
        create-lockfiles nil
        auto-save-default nil
        backup-directory-alist
        `(("." . ,(expand-file-name
                   (concat user-emacs-directory "backups")))))
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  ;; set text input and behaviour ----------------------------------------
  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  ;; tabs
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  ;; overwrite regions on keypress
  (delete-selection-mode t)
  ;; highlight current line
  (global-hl-line-mode t)
  (set-default 'indicate-buffer-boundaries '((top . nil) (bottom . right)))

  (setq truncate-partial-width-windows nil)
  (set-default 'truncate-lines 't)
  (setq-default word-wrap t)
  (toggle-truncate-lines -1)

  ;; scroll one line at a time
  (setq scroll-step 1
        ;; ...and don't go bananas when scrolling
        scroll-conservatively 10000)
  ;; Make it difficult to quit emacs
;;  (define-key ctl-x-map (kbd "C-S-c") 'save-buffers-kill-terminal)
;;  (define-key ctl-x-map (kbd "C-c") 'delete-frame)
  ;; Remove warnings when using certain commands
  (put 'narrow-to-region 'disabled nil)
  ;; auto load files when they change on disk
  (global-auto-revert-mode t)
  ;; remove whitespace when buffers are saved
  (add-hook 'before-save-hook 'whitespace-cleanup))

(when window-system
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; (use-package subr-x
;;   :config
;;   (put 'if-let   'byte-obsolete-info nil)
;;   (put 'when-let 'byte-obsolete-info nil))

(use-package auto-compile
  :straight t
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t
        auto-compile-source-recreate-deletes-dest t
        auto-compile-toggle-deletes-nonlib-dest t
        auto-compile-update-autoloads t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

;; set the os path
(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package custom
  :preface
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  :if (file-exists-p custom-file)
  :config
  (load custom-file))

(use-package server
  :straight t
  :config (or (server-running-p) (server-mode)))

;;; Long tail
(use-package ns
  :if (eq window-system 'ns)
  :config
  ;; mac keyboard
  (setq ;;mac-option-modifier 'super
      ;;mac-option-key-is-meta t
      mac-right-option-modifier nil)

  ;; the native fullscreen in macOS is annoying
  (setq ns-use-srgb-colorspace t)
  (setq ring-bell-function 'ignore)
  ;; disable osx native fullscreen
;;  (setq ns-use-native-fullscreen nil)
  (add-hook 'after-init-hook 'toggle-frame-fullscreen))

(use-package dash
  :straight t
  :config (dash-enable-font-lock))

(progn ; `isearch'
  :straight t
  (setq isearch-allow-scroll t))

(use-package recentf
  :straight t
  :demand t
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package savehist
  :straight t
  :config (savehist-mode))

(use-package saveplace
  :straight t
  :when (version< "25" emacs-version)
  :config (save-place-mode))

;; (use-package tramp
;;   :defer t
;;   :config
;;   (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
;;   (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
;;   (add-to-list 'tramp-default-proxies-alist
;;                (list (regexp-quote (system-name)) nil nil)))

(progn ; personalize
  (let* ((files (list "config/evil-mode"
                      "config/fundamental-mode"
                      "config/prog-mode"
                      user-real-login-name)) ; load USER-NAME.el
         (default-directory user-emacs-directory))
    (dolist (f files)
      (let ((file (expand-file-name (concat f ".el"))))
        (if (file-exists-p file)
          (progn (load file)
                 (message "Done loading config file: %s" file))
          (message "Please create file: %s" file))))))
(message "Your mother was a hamster, and your father smells like elderberries....")

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here

(switch-to-buffer "*Scratch*")
