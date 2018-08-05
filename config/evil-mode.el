(use-package evil
  :straight t
  :init
  (progn
    (setq evil-default-cursor t)
    (setq evil-want-integration nil)
    (add-hook 'evil-normal-state-entry-hook (lambda () (set-cursor-color "orange")))
    (add-hook 'evil-insert-state-entry-hook (lambda () (set-cursor-color "orange")))
    (add-hook 'evil-visual-state-entry-hook (lambda () (set-cursor-color "orange")))
    (add-hook 'evil-emacs-state-entry-hook (lambda () (set-cursor-color "green")))
    (add-hook 'evil-hook (lambda () (if (eq evil-state 'emacs)
                                        (set-cursor-color "green")
                                      (set-cursor-color "orange"))))
    (add-hook 'evil-operator-state-entry-hook (lambda () (set-cursor-color "orange")))
    (add-hook 'evil-replace-state-entry-hook (lambda () (set-cursor-color "orange")))
    (add-hook 'evil-motion-state-entry-hook (lambda () (set-cursor-color "orange")))


    )
  :config
  (evil-mode 1)
  )

(use-package evil-collection
  :straight t
  :config
  (evil-collection-init 'neotree)
  (evil-collection-init 'term)
  (evil-collection-init 'magit)
  (evil-collection-init 'company)
  (evil-collection-init 'dired)
  (evil-collection-init 'ivy)
  (evil-collection-init 'python)
  (evil-collection-init 'avy)
  )
