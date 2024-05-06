;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)
;; ignore native compile warning
(setq warning-minimum-level :emergency)
;(setq default-directory "/home/jordi/")
(defconst *IS-MAC* (eq system-type 'darwin))
(defconst *IS-LINUX* (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst *IS-WINDOWS*  (memq system-type '(cygwin windows-nt ms-dos)))

(when *IS-MAC*
  ;; modify meta from ⌥ to ⌘
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame))


;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time)


(require 'package)

(setq package-archives '(
                         ("melpa-stable" . "http://stable.melpa.org/packages/")			 
			 ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/") 
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org" . "https://orgmode.org/elpa/"))
			 ;; ("melpa" . "https://melpa.org/packages/"))
                          )

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(global-visual-line-mode t)
(use-package visual-fill-column
  :ensure t
  :init
  (visual-fill-column-mode))
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(setq-default visual-fill-column-center-text t)
(setq visual-fill-column-width 150)


(setq org-image-actual-width nil)

(defvar my-packages
  '(
    ;; colorful parenthesis matching
    rainbow-delimiters
    magit
    swiper
    clipetty
    bing-dict
    visual-fill-column))

(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; bing-dict
(use-package bing-dict :ensure t)
(global-set-key (kbd "C-c d") 'bing-dict-brief)
(setq bing-dict-vocabulary-save t)

;;clipetty
(use-package clipetty
  :ensure t
  :bind ("M-w" . clipetty-kill-ring-save))

;;load modules
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/customizations")

(require 'ui)
(require 'editing)
(require 'shell-integration)
(require 'navigation)
(require 'misc)



;; Hard-to-categorize customizations
(require 'misc)

;; Langauage-specific
;; (require 'elisp-editing)
(require 'init-org)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   '("5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" default))
 '(package-selected-packages
   '(hydra helpful ivy-prescient counsel which-key all-the-icons command-log-mode no-littering auto-package-update restclient docker-compose-mode docker telega marginalia orderless vertico keycast json-navigator json-mode doom ewal-doom-themes corfu magit tagedit rainbow-delimiters cider clojure-mode-extra-font-locking clojure-mode exec-path-from-shell))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

