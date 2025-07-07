(require 'treesit)

(setq treesit-extra-load-path '("~/codebase/src/tree-sitter-module/dist/"))
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; Automatic Tree-sitter grammar management
(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :init (setq treesit-auto-install 'prompt))

;; (use-package treesit-fold
;;   :defer nil
;;   :hook (emacs-lisp-mode . (lambda () (treesit-parser-create 'elisp)))
;;   )

;; Code folding indicators using Tree-sitter
(use-package treesit-fold-indicators
  :ensure treesit-fold
  :hook (after-init . global-treesit-fold-indicators-mode)
  :init (setq treesit-fold-indicators-priority -1))

(use-package s
  :vc (:url "https://github.com/magnars/s.el" :branch master))


(global-set-key "\C-h\C-f" 'find-function-at-point)

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
         ;; NOTE: Disable in large files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         ;; (find-file . (lambda ()
         ;;                (when (too-long-file-p)
         ;;                  (aggressive-indent-mode -1))))
         )
  :config
  ;; Disable in some modes
  (dolist (mode '(gitconfig-mode
                  asm-mode web-mode html-mode
                  css-mode css-ts-mode
                  go-mode go-ts-mode
                  python-ts-mode yaml-ts-mode
                  scala-mode
                  shell-mode term-mode vterm-mode
                  prolog-inferior-mode))
    (add-to-list 'aggressive-indent-excluded-modes mode))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c-mode 'c++-mode 'csharp-mode
                                     'java-mode 'go-mode 'swift-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line)))))
  )


;; [indent-bars] Show indent guides
(use-package indent-bars
  :ensure t
  :hook (prog-mode . indent-bars-mode)
  :config
  (setq indent-bars-display-on-blank-lines nil
        indent-bars-width-frac 0.2
        indent-bars-color '(highlight :face-bg t :blend 0.2)
        indent-bars-zigzag nil
        indent-bars-highlight-current-depth nil
        indent-bars-pattern "."
        indent-bars-prefer-character t)
  )

;; usage for cursor movement see [https://ebzzry.com/en/emacs-pairs/#configuration]
;; (use-package smartparens-config
;;   :ensure smartparens
;;   :hook ((prog-mode org-mode) . turn-on-smartparens-strict-mode)
;;   :config (progn (show-smartparens-global-mode t)))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))


(provide 'init-progn)
