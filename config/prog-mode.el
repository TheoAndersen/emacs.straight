;; (use-package prog-mode
;;   :config
;;   (global-prettify-symbols-mode))

(use-package prettier-js
  :straight t
  :config
  (setq prettier-js-args '
        ("--no-semi"
        "--single-quote" "--print-width 120" ))
  )


(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package flycheck-color-mode-line
  :straight t
  )

(use-package evil-smartparens
  :straight t
  :config
  (show-smartparens-global-mode))


(use-package highlight-indent-guides
  :straight t
  ;; lines at indents
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
)

(defun bury-compile-buffer-if-successful (buffer string)
 "Bury a compilation buffer if succeeded without warnings "
 (when (and
         (buffer-live-p buffer)
         (string-match "elm-make" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-with-timer 0 nil
                    (lambda (buf)
                      (bury-buffer buf)
                      (switch-to-prev-buffer (get-buffer-window buf) 'kill)
                      (delete-window (get-buffer-window buf)))


                    buffer)))

(use-package elm-mode
  :straight t
  :mode "\\.elm\\'"
  :bind (("M-." . elm-mode-goto-tag-at-point)
         ("M-," . pop-tag-mark))
  :preface
  (defun mg/my-elm-mode-hook ()
    (flycheck-mode 1)
    (smartparens-mode 1))
  :hook ((elm-mode . mg/my-elm-mode-hook))
  :config
  ;; (add-hook 'elm-mode-hook 'elm-oracle-setup-completion)
  ;; (add-hook 'elm-mode-hook 'elm-mode-generate-tags)
  (progn

    ;; (add-hook 'elm-mode-hook 'elm-oracle-setup-completion)
    (add-to-list 'company-backends '(company-elm :with company-dabbrev))
    (defvar elm-compile-arguments '("--yes" "--output=elm.js"))
    (setq elm-format-on-save 't)
    ;; (setq elm-tags-on-save 't)
    ;; (setq elm-tags-exclude-elm-stuff 't)
    (setq flycheck-display-errors-delay 0.1)
    (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)
    )
  )
(use-package flycheck-elm
  :straight t
  :after (flycheck elm-mode)
  :init
  (setq flycheck-elm-reporting-mode 'all)
  :config
  (flycheck-elm-setup)
  )

(use-package vue-mode
  :straight t
  :mode "\\.vue\\'"
  :hook ((vue_mode . prettier-js-mode)) ;; todo, this dosent quite work yet
)
(defun goto-def-or-rgrep ()
  "Go to definition of thing at point or do an rgrep in project if that fails"
  (interactive)
  (condition-case nil (elpy-goto-definition)
    (error (elpy-rgrep-symbol (thing-at-point 'symbol)))))

(use-package tide
  :straight t
  :mode "\\.ts\\'"
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package groovy-mode
  :straight t
  :mode "\\.groovy\\'"
  )

(use-package elpy
  :straight t
  :config
  (progn
    ;; Use Flycheck instead of Flymake
    (when (require 'flycheck nil t)
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (remove-hook 'elpy-modules 'elpy-module-yasnippet)
      ;; (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
      (add-hook 'elpy-mode-hook 'flycheck-mode))
    (flycheck-mode +1)
    (delete `elpy-module-highlight-indentation elpy-modules)

    (elpy-enable)
    (define-key elpy-mode-map (kbd "M-.") 'goto-def-or-rgrep)
    (define-key evil-motion-state-map "gd" 'goto-def-or-rgrep)
    (setq elpy-rpc-backend "jedi")
    ;; (add-hook 'venv-postactivate-hook 'jedi:stop-server)
    ;; (add-hook 'venv-postdeactivate-hook 'jedi:stop-server)
    (add-hook 'python-mode-hook
              (lambda ()
                ;; explicitly load company for the occasion when the deferred
                ;; loading with use-package hasn't kicked in yet
                (company-mode)))
    (company-mode +1)
    ;; (setq ansi-color-for-comint-mode t)
    (add-hook 'elpy-mode-hook
              '(lambda ()
                 (when (eq major-mode 'python-mode)
                   (add-hook 'before-save-hook 'elpy-black-fix-code))))
    (setq elpy-rpc-python-command "python3")

    (setq python-shell-interpreter "python3")
    ;;           python-shell-interpreter-args "--simple-prompt -i")
    ;; (require 'py-autopep8)
    ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
    (setq python-shell-prompt-detect-failure-warning nil) ;; to avoid that annoying warning on compile
    )
  )
