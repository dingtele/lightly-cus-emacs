


;; ;; (defun efs/org-font-setup ()
;; ;;   ;; Replace list hyphen with dot
;; ;;   (font-lock-add-keywords 'org-mode
;; ;;                           '(("^ *\\([-]\\) "
;; ;;                              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; ;;   ;; Set faces for heading levels
;; ;;   (dolist (face '((org-level-1 . 1.2)
;; ;;                   (org-level-2 . 1.1)
;; ;;                   (org-level-3 . 1.05)
;; ;;                   (org-level-4 . 1.0)
;; ;;                   (org-level-5 . 1.1)
;; ;;                   (org-level-6 . 1.1)
;; ;;                   (org-level-7 . 1.1)
;; ;;                   (org-level-8 . 1.1)))
;; ;;     (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))
;; ;; ;; Ensure that anything that should be fixed-pitch in Org files appears that way
;; ;;   (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
;; ;;   (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
;; ;;   (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
;; ;;   (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
;; ;;   (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
;; ;;   (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;; ;;   (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;; ;;   (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;; ;;   (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
;; ;;   (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
;; ;;   (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

;; ;;自动折行
;; (setq truncate-lines nil) 


;; (use-package org-bullets
;;   :hook (org-mode . org-bullets-mode)
;;   :custom
;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))



;; ;; Org mode的附加包，有诸多附加功能
;; (use-package org-contrib
;;   :ensure t)






;; ;; (use-package corg
;;   ;; :ensure t
;;   ;; :hook (org-mode . #'corg-setup))

;; (customize-set-variable 'org-anki-default-deck "org-deck")



;; Minimal UI
;; (package-initialize)
;; (menu-bar-mode -1)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)
;; ;; (modus-themes-load-operandi)

;; ;; Choose some fonts
;; ;; (set-face-attribute 'default nil :family "Iosevka")
;; ;; (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
;; ;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

;; ;; Add frame borders and window dividers
;; (modify-all-frames-parameters
;;  '((right-divider-width . 40)
;;    (internal-border-width . 40)))
;; (dolist (face '(window-divider
;;                 window-divider-first-pixel
;;                 window-divider-last-pixel))
;;   (face-spec-reset-face face)
;;   (set-face-foreground face (face-attribute 'default :background)))
;; (set-face-background 'fringe (face-attribute 'default :background))



;; (global-org-modern-mode)




(provide 'init-org)
