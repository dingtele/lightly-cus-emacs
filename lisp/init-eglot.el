;; Sets up exec-path-from shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p
                                 'emacs-lisp-mode 'lisp-mode
                                 'makefile-mode 'snippet-mode
                                 'ron-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure)
         (python-mode . eglot-ensure)
         (clojure-mode . eglot-ensure)
         (clojure-ts-mode . eglot-ensure)
         (clojure-ts-clojurescript-mode . eglot-ensure)
         (eglot-managed-mode . my/eglot-capf)
         )
  :init
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.5)
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(clojure-mode . ("clojure-lsp")))
  (add-to-list 'eglot-server-programs '(clojure-ts-mode . ("clojure-lsp")))
  (add-to-list 'eglot-server-programs '(clojure-ts-clojurescript-mode . ("clojure-lsp")))

  (defun my/eglot-capf ()
    "Set custom completion-at-point functions for Eglot."
    (setq-local completion-at-point-functions '(eglot-completion-at-point)))
  )

(use-package consult-eglot
  :after consult eglot
  :bind (:map eglot-mode-map
         ("C-M-." . consult-eglot-symbols)))

;; Emacs LSP booster
(use-package eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :ensure nil
  :init (unless (package-installed-p 'eglot-booster)
          (package-vc-install "https://github.com/jdtsmith/eglot-booster"))
  :hook (after-init . eglot-booster-mode))


(provide 'init-eglot)
