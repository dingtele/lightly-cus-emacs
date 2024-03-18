
(setq debug-on-error 1)
(setq default-directory "/home/jordi/")
(require 'package)

(setq package-archives '(
                         ;("melpa-stable" . "http://stable.melpa.org/packages/")			 
			 ;("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/") 
                        ; ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("gnu"   . "http://1.15.88.122/gnu/")
                         ("nongnu" . "http://1.15.88.122/nongnu/")
			 ("melpa" . "http://1.15.88.122/mlpea/")))

(package-initialize)

;;install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Define he following variables to remove the compile-log warnings
;; when defining ido-ubiquitous
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(defvar predicate nil)
(defvar inherit-input-method nil)      

(defvar my-packages
  '(
    ;; colorful parenthesis matching
    paredit
    rainbow-delimiters
    magit
    bing-dict
    swiper
clipetty
bing-dict
    ))

(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; bing-dict
(use-package bing-dict :ensure t)
(global-set-key (kbd "C-c d") 'bing-dict-brief)
;; (setq bing-dict-vocabulary-file "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/emacs-bing-vocabulary.org") 
(setq bing-dict-vocabulary-save t)
;;clipetty
(use-package clipetty
  :ensure t
  :bind ("M-w" . clipetty-kill-ring-save))

(global-visual-line-mode t)
(use-package visual-fill-column :ensure t)
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(setq-default visual-fill-column-center-text t)
(setq org-image-actual-width nil)

(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/customizations")
 
(load "shell-integration.el")
(load "navigation.el")
(load "ui.el")
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; Langauage-specific
(load "elisp-editing.el")
(load "init-org.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" default))
 '(package-selected-packages
   '(doom ewal-doom-themes corfu magit tagedit rainbow-delimiters projectile smex ido-completing-read+ cider clojure-mode-extra-font-locking clojure-mode paredit exec-path-from-shell))
 '(visual-fill-column-width 120)
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

