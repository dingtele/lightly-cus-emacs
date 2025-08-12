;;; init-prog.el --- summary -*- lexical-binding: t -*-

;; Author: madcomet
;; Maintainer: madcomet
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

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


;; Show function arglist or variable docstring
(use-package eldoc
  :ensure nil
  :diminish
  :config
  (use-package eldoc-box
    :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
    :custom
    (eldoc-box-lighter nil)
    (eldoc-box-only-multi-line t)
    (eldoc-box-clear-with-C-g t)
    :custom-face
    (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
    (eldoc-box-body ((t (:inherit tooltip))))
    :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
    :config
    ;; Prettify `eldoc-box' frame
    (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
          (alist-get 'right-fringe eldoc-box-frame-parameters) 8)))

;; Cross-referencing commands
(use-package xref
  ;; :bind (("M-g ." . xref-find-definitions)
  ;;        ("M-g ," . xref-go-back))
  :init
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))
  )

(provide 'init-progn)

;;; init-prog.el ends here
