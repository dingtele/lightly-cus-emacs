;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

;; Allow access from emacsclient
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (require 'server)
;;             (unless (server-running-p)
;;               (server-start))))

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
                         ;; ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ;; ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ;; ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                          ;; ("nongnu" . "https://elpa.nongnu.org/nongnu/")
))



(package-initialize)


(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

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


;;clipetty
(use-package clipetty
  :ensure t
  :bind ("M-w" . clipetty-kill-ring-save))

;;load modules
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/customizations")
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/org-roam/")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/org-roam/extensions/")
;; (require 'org-roam)

(require 'ui) ;; -> F4
(require 'editing) ;; -> F2
(require 'shell-integration)
(require 'navigation) ;; -> F3
(require 'misc)
(require 'init-site-lisp)
(require 'init-core-overriding)
;; Langauage-specific
(provide 'init-eglot)
;; (require 'elisp-editing)
(require 'init-org)
(org-babel-load-file "/home/madcomet/.emacs.d/customizations/tools.org")

;; (require 'tools)
(require 'init-minibuffer-completion)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(ef-trio-light))
 '(eglot-java-server-install-dir "~/.emacs.d/share/eclipse.jdt.ls")
 '(package-selected-packages
   '(fringe-helper editorconfig eglot-java eask tree-sitter-langs justify-kp flx-ido flx auto-complete company ejc-sql ox-hugo mct org-zettel-ref-mode yasnippet lsp-java dap-mode lsp-treemacs lsp-ivy helm-lsp lsp-ui lsp-mode rg nerd-icons-ivy-rich ibuffer-vc ibuffer-projectile ibuffer-project nerd-icons-ibuffer workgroups2 org-remark-info nerd-icons-completion consult pinyinlib org-remark buffer-name-relative spacemacs-theme haskell-mode markdown-mode popper capf-autosuggest which-key vc-use-package use-package ts-fold toc-org telega tagedit rime restclient rainbow-delimiters org-preview-html org-contrib org-bullets org-appear orderless nov no-littering memoize magit keycast json-navigator json-mode ivy-rich ivy-prescient immersive-translate hydra helpful gruvbox-theme groovy-mode gptel gnu-elpa-keyring-update exec-path-from-shell ewal-doom-themes evil-nerd-commenter eshell-syntax-highlighting ef-themes doom-modeline doom docker-compose-mode docker dimmer datetime counsel corfu command-log-mode clipetty calibredb bing-dict auto-package-update))
 '(package-vc-selected-packages
   '((eaf :vc-backend Git :url "https://github.com/emacs-eaf/emacs-application-framework")
     (justify-kp :vc-backend Git :url "https://github.com/Fuco1/justify-kp")
     (org-zettel-ref-mode :vc-backend Git :url "https://github.com/yibie/org-zettel-ref-mode")
     (buffer-name-relative :vc-backend Git :url "https://codeberg.org/ideasman42/emacs-buffer-name-relative")
     (ts-fold :vc-backend Git :url "https://github.com/emacs-tree-sitter/ts-fold"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'scroll-left 'disabled nil)
