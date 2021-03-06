;; (use-package material-theme
;;   :config
;;   (setq custom-safe-themes t)
;;   (load-theme 'material)
;;   ;; Remove the background box on headlines in org-mode buffers
;;   (let ((headline `(:background nil :box nil))
;;         (class '((class color) (min-colors 89))))
;;     (custom-theme-set-faces
;;      'user
;;      ;; `(org-level-4 ((t (:height 1.0))))
;;      ;; `(org-level-3 ((t (:height 1.0))))
;;      ;; `(org-level-2 ((t (:height 1.0))))
;;      ;; `(org-level-1 ((t (:height 1.0))))
;;      ;; `(org-document-title ((t (,@headline :height 1.25 :underline nil))))
;;      ;; `(org-block-begin-line ((t (:box nil))))
;;      ;; `(org-block-end-line ((t (:box nil))))
;;      ;; `(org-block ((t (:foreground nil :background "#1e2930"))))
;;      ;; `(org-code ((t (:background "#1e2930"))))
;;      ;; `(flycheck-error ((t (:background "#5E1E11"))))
;;      ;; I find this rainbow-delimiters scheme better for non-lisp modes:
;;      `(rainbow-delimiters-depth-1-face ((,class (:foreground ,"#C0C0C0"))))
;;      `(rainbow-delimiters-depth-2-face ((,class (:foreground ,"#B388FF"))))
;;      `(rainbow-delimiters-depth-3-face ((,class (:foreground ,"#26A69A"))))
;;      `(rainbow-delimiters-depth-4-face ((,class (:foreground ,"#EF6C00"))))
;;      `(rainbow-delimiters-depth-5-face ((,class (:foreground ,"#2196F3"))))
;;      `(rainbow-delimiters-depth-6-face ((,class (:foreground ,"#FFCDD2"))))
;;      `(rainbow-delimiters-depth-7-face ((,class (:foreground ,"#8BC34A"))))
;;      `(rainbow-delimiters-depth-8-face ((,class (:foreground ,"#FFF59D"))))
;;      `(rainbow-delimiters-depth-9-face ((,class (:foreground ,"#795548"))))
;;      `(rainbow-delimiters-unmatched-face ((,class (:foreground ,"#F0F0F0" :background ,"#F36C60"))))
;;     )))

(use-package seti-theme
  :straight t
  :config
  (setq custom-safe-themes t)
  (load-theme 'seti)
  (set-face-background 'highlight "darkblue")
  ;; (set-face-foreground 'highlight "yellow")
  )

(when (eq window-system 'ns)
  ;; @todo, set font size based on screen resoulution, not system-name
  ;; set fonts
  (if (string-equal (system-name) "carton.local")
      (let ((font "source code pro"))
        (message "On mac with computer carton.local")
        (set-frame-font font)
        (set-face-font 'default font)
        (set-face-attribute 'default nil :height 135)
        (set-face-attribute 'mode-line nil :font font :height 90)
        (set-face-attribute 'mode-line-inactive nil :font font :height 90)
        (set-face-attribute 'term nil :height 90))

        ;; (set-face-attribute 'ansi-term nil :font font :height 90))
    ;; else
    (let ((font "source code pro"))
      (message "On mac, but dunno which machine")
      (set-frame-font font)
      (set-face-font 'default font)
      (set-face-attribute 'default nil :height 135)
      (set-face-attribute 'mode-line nil :font font :height 90)
      (set-face-attribute 'mode-line-inactive nil :font font :height 90)
      (set-face-attribute 'term nil :font font :height 100))
    ))
(when (eq window-system 'x)
  (message "On linux")
  (let ((font "source code pro"))
    (set-frame-font font)
    (set-face-font 'default font)
    (set-face-attribute 'default nil :height 110)
    (set-face-attribute 'mode-line nil :font font :height 80)
    (set-face-attribute 'mode-line-inactive nil :font font :height 80)
    (set-face-attribute 'term nil :font font :height 100))
  )
(custom-set-faces
     (set-face-attribute 'neo-button-face      nil :height 115)
     (set-face-attribute 'neo-file-link-face   nil :height 115)
     (set-face-attribute 'neo-dir-link-face    nil :height 115)
     (set-face-attribute 'neo-header-face      nil :height 115)
     (set-face-attribute 'neo-expand-btn-face  nil :height 115)
     (set-face-attribute 'neo-banner-face      nil :height 115)
     (set-face-attribute 'neo-root-dir-face    nil :height 115)
     )

(when (eq window-system 'w32)
  (message "On Windows")
  (let ((font "source code pro"))
    (set-frame-font font)
    (set-face-font 'default font)
    (set-face-attribute 'default nil :height 110)
    (set-face-attribute 'mode-line nil :font font :height 80)
    (set-face-attribute 'mode-line-inactive nil :font font :height 80))
  )
