
(use-package hl-todo
  :ensure t
  :defer t
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("PERF" . "#4EEE85")
          ("FIXME"  . "#FF0000")
          ("DEBUG-ON-QUIT"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("NTC"   . "#1E90FF"))) ;;short for NOTICE
  (global-hl-todo-mode))


(use-package paren
  :custom-face (show-paren-match ((t (:foreground "SpringGreen3" :underline t :weight bold))))
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen 'overlay ;; FIXME not working yet
        blink-matching-paren-highlight-offscreen t
        show-paren-delay 0.2)
  )

;; [rainbow-delimiters] Highlight brackets according to their depth
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook ((prog-mode conf-mode yaml-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5))

(use-package highlight-parentheses
  :ensure t
  :defer t
  :hook ((minibuffer-setup . highlight-parentheses-minibuffer-setup)
         (prog-mode . highlight-parentheses-mode))
  :config
  (setq highlight-parentheses-colors '("firebrick1" "firebrick3" "orange1" "orange3")
        highlight-parentheses-attributes '((:underline t) (:underline t) (:underline t))
        highlight-parentheses-delay 0.2)
  )

(use-package hl-line
  :hook (after-init . global-hl-line-mode)
  :config
  ;; (setq hl-line-sticky-flag nil)
  ;; ;; Highlight starts from EOL, to avoid conflicts with other overlays
  ;; (setq hl-line-range-function (lambda () (cons (line-end-position)
  ;;                                          (line-beginning-position 2))))
  )

(use-package region-occurrences-highlighter
  :ensure t
  :defer t
  :config
  (add-hook 'prog-mode-hook #'region-occurrences-highlighter-mode)
  (add-hook 'org-mode-hook #'region-occurrences-highlighter-mode)
  (add-hook 'text-mode-hook #'region-occurrences-highlighter-mode)
  (define-key region-occurrences-highlighter-nav-mode-map "\M-n" 'region-occurrences-highlighter-next)
  (define-key region-occurrences-highlighter-nav-mode-map "\M-p" 'region-occurrences-highlighter-prev))


(use-package colorful-mode
  :ensure t ; Optional
  :defer t
  :hook (prog-mode text-mode)
  ;; :config (global-colorful-mode) ; Enable it globally
  ...)

;; (defun exec/lsp-mode-string()
;;   (concat
;;    (propertize " eglot "
;;                'face '(:foreground "white" :background "brown"))
;;    (propertize
;;     (format (if (derived-mode-p 'eglot-mode)
;;                 " on  "" off "))
;;     'face '(:foreground "white" :background "gray40"))))

;; (add-to-list 'header-line-format '(:eval (exec/lsp-mode-string)) t)

;; (setq-default header-line-format  '("" keycast-header-line (:eval (exec/lsp-mode-string))))


(provide 'init-highlight)
