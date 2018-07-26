(use-package evil
  :straight t
  :init
  (progn
    (setq evil-default-cursor t)
    (add-hook 'evil-normal-state-entry-hook (lambda () (set-cursor-color "orange")))
    (add-hook 'evil-insert-state-entry-hook (lambda () (set-cursor-color "orange")))
    (add-hook 'evil-visual-state-entry-hook (lambda () (set-cursor-color "orange")))
    (add-hook 'evil-emacs-state-entry-hook (lambda () (set-cursor-color "green")))
    (add-hook 'evil-operator-state-entry-hook (lambda () (set-cursor-color "orange")))
    (add-hook 'evil-motion-state-entry-hook (lambda () (set-cursor-color "orange")))
    (evil-mode 1)
    )
  )
