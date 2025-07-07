;; eshell
(use-package xterm-color
  :commands (xterm-color-filter))
(use-package eshell
  :defer t
  :after xterm-color
  :config
  (setq eshell-scroll-to-bottom-on-input t)
  (define-key eshell-mode-map (kbd "<tab>") #'company-complete)
  (define-key eshell-hist-mode-map (kbd "M-r") #'consult-history)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-before-prompt-hook (setq xterm-color-preserve-properties t))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))


;; (use-package eshell
;;    :config
;;    (setq eshell-scroll-to-bottom-on-input t)
;;    (setq-local tab-always-indent 'complete)
;;    (setq eshell-history-size 10000)
;;    (setq eshell-save-history-on-exit t) ;; Enable history saving on exit
;;    (setq eshell-hist-ignoredups t) ;; Ignore duplicatesq
;;    :hook
;;    (eshell-mode . my/eshell-hook))

(use-package capf-autosuggest
  :hook
  (eshell-mode . capf-autosuggest-mode))

(defun my/shell-create (name)
  "Create a custom-named eshell buffer with NAME."
  (interactive "sName: ")
  (eshell 'new)
  (let ((new-buffer-name (concat "*eshell-" name "*")))
    (rename-buffer new-buffer-name t)))

(global-set-key (kbd "C-c s") #'my/shell-create)



(provide 'init-eshell)
