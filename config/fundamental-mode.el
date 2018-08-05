(use-package simple
  :config
  ;; mode-line
  (setq uniquify-buffer-name-style 'forward)
  (column-number-mode))

;; shackle
(use-package shackle
  :straight t
  :preface
  (defun mg/add-shackle-rule (rule) (add-to-list 'shackle-rules rule))
  :config
  (setq shackle-rules
        '(("*Apropos*" :select t :align below :size 0.5)
          ("*Buffer List*" :select t :align below :size 0.33)
          ("*Help*" :select t :align below :size 0.5)
          ("*compilation*" :align right :size 0.5)
          ("*elm-make*" :align right :size 0.5)
          ))
  (shackle-mode))

(use-package swiper
  :straight t
  :config
  (ivy-mode 1))

(use-package ivy
  :straight t
  :config
  (setq ivy-use-selectable-prompt t
        ;; don't show recent closed items in various buffers
        ivy-use-virtual-buffers nil))

(use-package counsel
  :straight t
  :after ivy
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x))

(use-package avy
  :straight t
  :bind (("M-g SPC" . avy-goto-char)
         ("M-g w" . avy-goto-word-1)
         ("M-g l" . avy-goto-line)))

;;
;; Crux - A Collection of Ridiculously Useful eXtensions for Emacs
;;
(use-package crux
  :straight t
  :demand t
  :bind (("C-a" . crux-move-beginning-of-line)))


;; compilation
(use-package compile
  :straight t
  :preface
  (defun mg/my-compilation-mode-hook ()
    (setq compilation-scroll-output 'first-error)
    (text-scale-set -3))
  :hook ((compilation-mode . mg/my-compilation-mode-hook))
  :config
  (ignore-errors
    (require 'ansi-color)
    (add-hook 'compilation-filter-hook
              (lambda ()
                (when (eq major-mode 'compilation-mode)
                  (ansi-color-apply-on-region compilation-filter-start (point-max)))))
    (add-hook 'next-error-hook 'recenter)))

(use-package ripgrep
  :straight t
  )

(use-package projectile
  :straight t
  :preface
  (defun mg/update-projectile-project-list ()
    "Discover projects in `~/Development/github.com' and
`~/Development/gitlab.com' and add them to the project list used
by the Projectile project switcher"
    (interactive)
    ;; Perform cleanup before adding projects
    (projectile-cleanup-known-projects)
    ;;Find the projects in the structure and add them
    (let* ((default-directory "~/projects/")
           (project-site-globs '("*" "*/*" "github.com/*/*" "gitlab.com/*/*")))
      (dolist (project-site-glob project-site-globs)
        (let ((projects-glob (expand-file-name project-site-glob)))
          (dolist (project (file-expand-wildcards projects-glob))
            (if (not (string-match-p (regexp-quote "DS_Store") project))
                (projectile-add-known-project project))))))
    ;;Add my Emacs config folder as well ...
    (projectile-add-known-project "~/.emacs.d/"))
  :init
  (progn
    (setq projectile-enable-caching t)
    )
  :config
  (mg/update-projectile-project-list)
  (projectile-mode)
    (add-to-list 'projectile-globally-ignored-directories "elpa")
    (add-to-list 'projectile-globally-ignored-directories ".cache")
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (add-to-list 'projectile-globally-ignored-directories "deps")
    (add-to-list 'projectile-globally-ignored-directories "_build")
    (add-to-list 'projectile-globally-ignored-files "#*.*")
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (add-to-list 'projectile-globally-ignored-directories ".DS_Store")
    (add-to-list 'projectile-globally-ignored-directories "TAGS")
    (add-to-list 'projectile-globally-ignored-directories ".*")
  )

(use-package counsel-projectile
  :straight t
  :after (counsel projectile)
  :demand t
  :commands counsel-projectile-find-file
  :preface
  (defun universal-argument-find-file ()
    "wrap the `find-file'-command, bound to `C-x C-f', with a
check for whether or not the universal argument has been applied,
and how many times.
Zero times: normal behavior (find file); Once: find file in
project; Twice: find/open project"
    (interactive)
    (cond ((equal current-prefix-arg nil)
           (call-interactively 'find-file))
          ((equal current-prefix-arg (list 4))
           (counsel-projectile-find-file))
          ((equal current-prefix-arg (list 16))
           (projectile-switch-project))
          ))
  (defun universal-argument-switch-to-buffer ()
    "wrap the `switch-to-buffer'-command, bound to `C-x b', with a
check for whether or not the universal argument has been applied,
and how many times.
Zero times: normal behavior (ivy-switch-buffer); Once: switch to
buffer in project/erc; twice to switch between open projects."
    (interactive)
    (cond ((and (equal current-prefix-arg (list 4)) (equal major-mode 'erc-mode))
           (call-interactively 'mg/erc-switch-to-buffer))
          ((and (equal current-prefix-arg (list 4)) (projectile-project-p))
           (call-interactively 'counsel-projectile-switch-to-buffer))
          ((equal current-prefix-arg (list 16))
           (projectile-switch-open-project))
          (t (call-interactively 'switch-to-buffer))
          ))
  (defun universal-argument-kill-buffer ()
    "wrap the `kill-buffer'-command, bound to `C-x k', with a
check for whether or not the universal argument has been applied
or not.
Zero times: normal behavior (kill-buffer);
Once: (projectile-kill-buffers)"
    (interactive)
    (cond ((equal current-prefix-arg nil)
           (call-interactively 'kill-buffer))
          ((equal current-prefix-arg (list 4))
           (call-interactively 'projectile-kill-buffers))
          ))
  :bind (:map ctl-x-map
              ("C-f" . universal-argument-find-file)
              ("C-b" . universal-argument-switch-to-buffer)
              ("k" . universal-argument-kill-buffer)
              ("p s" . projectile-ripgrep))
  :bind (("C-c p p" . counsel-projectile)
         ("C-c p s s" . counsel-projectile-rg)
         ("C-c p s r" . counsel-projectile-rg)
         ("C-c p f" . counsel-projectile-find-file)
        )
  :config
  (setq projectile-completion-system 'ivy)
  ;; add directories and files to the projectile ignore list
  (add-to-list 'projectile-globally-ignored-directories "_build")
  (add-to-list 'projectile-globally-ignored-directories "deps")
  (add-to-list 'projectile-globally-ignored-file-suffixes ".beam"))

;;
;; Flycheck
;;
(use-package flycheck-color-mode-line
  :straight t
  :after (flycheck))

(use-package flycheck-pos-tip
  :straight t
  :after (flycheck))

;; (use-package flycheck-popup-tip
;;   :after (flycheck))

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(setq text-scale-mode-step 1.05)

(use-package flycheck
  :straight t
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (progn
    (setq flycheck-highlighting-mode 'symbols)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
    (add-hook 'flycheck-mode-hook 'flycheck-pos-tip-mode)
    (set-face-background 'flycheck-error "#660000")
    (set-face-foreground 'flycheck-error nil)
    (set-face-background 'flycheck-warning "#775500")
    (set-face-foreground 'flycheck-warning nil)
    (require 'flycheck-color-mode-line)
    (set-face-background 'flycheck-color-mode-line-error-face "#660000")
    (set-face-background 'flycheck-color-mode-line-warning-face "#553300")
    (set-face-background 'flycheck-color-mode-line-info-face nil)
    (set-face-foreground 'flycheck-color-mode-line-error-face nil)
    (set-face-foreground 'flycheck-color-mode-line-warning-face nil)
    (set-face-foreground 'flycheck-color-mode-line-info-face nil)))

;;s
;; Completion
;;
(use-package company
  :straight t
  :bind (("C-SPC" . company-complete))
  :config
  (progn
    (use-package company-statistics
      :straight t
      :config
      (company-statistics-mode 1))
    (global-company-mode 1)
    (setq company-idle-delay 0.3
          company-tooltip-limit 10
          company-minimum-prefix-length 2
          company-tooltip-align-annotations t
          company-tooltip-flip-when-above t)
    )
  )

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*term*")))

(use-package multi-term
  :straight t
  :config
  (progn
    (setq multi-term-program "/bin/bash")
    )
  :init
  (global-set-key [M-f9] 'multi-term)
  )
(defun shell-hook ()
   ;; (text-scale-decrease 1.5))
    (text-scale-set 1.8))
(add-hook 'shell-pop-in-hook 'shell-hook)

(use-package shell-pop
  :straight t
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/bash")
  (setq shell-pop-full-span 't)
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  (global-set-key [f9] 'shell-pop)
  )
;;
;; Git related
;;
(use-package magit
  :straight t
  :defer t
  :bind ((:map ctl-x-map
               ("g" . magit-status)
               ("M-g" . magit-dispatch-popup)))
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package diff-hl
  :straight t
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

;;
;;
;;
(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)
         ("C-M-=" . er/contract-region)))

;;
;; Dired
;;
(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

;;
;; Help systems
;;
(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(use-package man
  :defer t
  :config
  (setq Man-width 80))

;;
;; Eshell - the best shell in existence
;;
(use-package eshell
  :straight t
  :after projectile
  :preface
  (defun universal-argument-eshell ()
    "wrap the `eshell'-command, with a check for whether or not
the universal argument has been applied, and how many times.
Zero times: normal behavior (eshell); Once: open a shell in the
current project root"
    (interactive)
    (cond ((equal current-prefix-arg nil)
           (call-interactively 'eshell))
          ((equal current-prefix-arg (list 4))
           (projectile-run-eshell))
          ))
  ;; mode hook
  (defun mg/my-eshell-mode-hook ()
    (set (make-local-variable 'global-hl-line-mode) nil))
  :hook (eshell-mode . mg/my-eshell-mode-hook)
  :bind
  ((:map ctl-x-map
         ("C-t" . universal-argument-eshell))))

;(define-key global-map (kbd "C-+") 'zoom-frm-in)
;(define-key global-map (kbd "C--") 'zoom-frm-out)
;
(define-key global-map (kbd "M-<f11>") 'toggle-frame-fullscreen)

;; Jump fast between open windows
(use-package ace-window
  :straight t
  :config
  :bind ("C-c o" . ace-window)
  )

;; Jump fast between buffers
(use-package ace-jump-buffer
  :straight t
  :config
  :bind ("C-c b" . ace-jump-buffer))


(use-package all-the-icons
  :straight t
  :config
  (setq all-the-icons-scale-factor 0.9)
  )

(use-package neotree
  :straight t
  :config
  (progn
    (global-set-key [f8] 'neotree-toggle)
    (setq neo-autorefresh nil)
    (setq neo-theme 'icons) ;; not all icons are alighed properly :(

    )
  )

;; (use-package all-the-icons-ivy
;;   :straight t
;;   :config
;;   (all-the-icons-ivy-setup))

;; (use-package all-the-icons-dired
;;   :straight t
;;   :hook (dired-mode . all-the-icons-dired-mode)
;;   :config
;;   )

(use-package logview
  :straight t
  )
