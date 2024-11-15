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
                         ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ;; ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ;; ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ;; ("org" . "https://orgmode.org/elpa/"
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

(setq org-image-actual-width nil)

(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

;; Auto install the required packages
;; Set missing package vars
(defvar lem-missing-packages '()
  "List populated at startup containing packages needing installation.")
(defvar lem-missing-vc-packages '()
  "List populated at startup containing vc packages requiring installation.")

;; Check for packages
(defun lem-check-missing-packages ()
  "Check for missing packages."
  (interactive)
  ;; Check packages
  (message "%s" "Checking for missing packages.")
  (dolist (p package-selected-packages)
    (unless (package-installed-p p)
      (add-to-list 'lem-missing-packages p 'append)))
  ;; Check vc installed packages (Emacs 29+)
  (when (version< "29" emacs-version)
    (message "%s" "Checking for missing vc packages.")
    (dolist (p package-vc-selected-packages)
      (unless (package-installed-p (car p))
        (add-to-list 'lem-missing-vc-packages (car p) 'append)))))

;; Install packages
(defun lem-install-missing-packages ()
  "Install missing packages from package & package-vc lists."
  (interactive)
  (lem-check-missing-packages)
  (cond ((or lem-missing-packages
             lem-missing-vc-packages)
         (message "Refreshing package database & installing missing packages...")
         (package-install-selected-packages t)
         (setq lem-missing-packages '())
         (package-vc-install-selected-packages)
         (setq lem-missing-vc-packages '()))
        (t
         (message "No missing packages."))))


;;clipetty
(use-package clipetty
  :ensure t
  ;; :bind ("M-w" . clipetty-kill-ring-save)
  )

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-editing-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/editing.el"))

(defun open-navigation-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/navigation.el"))

(defun open-ui-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/ui.el"))

(defun open-init-org-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/init-org.el"))

(defun open-misc-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/misc.el"))

(defun open-tools-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/tools.org"))

(global-set-key (kbd "<f1>") 'open-init-file)
(global-set-key (kbd "<f2>") 'open-editing-file)
(global-set-key (kbd "<f3>") 'open-navigation-file)
(global-set-key (kbd "<f4>") 'open-ui-file)
(global-set-key (kbd "<f5>") 'open-init-org-file)
(global-set-key (kbd "<f6>") 'open-misc-file)
(global-set-key (kbd "<f9>") 'open-tools-file)

;;load modules
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/customizations")
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(require 'editing) ;; -> F2
(require 'ui) ;; -> F4
(require 'shell-integration)
(require 'navigation) ;; -> F3
(require 'misc)
(require 'init-site-lisp)
(require 'init-core-overriding)
;; Langauage-specific
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
 '(custom-enabled-themes '(doom-tokyo-night))
 '(org-bullets-bullet-list nil)
 '(package-selected-packages
   '(anki-helper auto-complete auto-package-update bind-key bing-dict
                 buffer-name-relative calibredb capf-autosuggest
                 clipetty cljsbuild-mode clojure-ts-mode
                 command-log-mode company corfu counsel dash datetime
                 dimmer docker docker-compose-mode doom doom-modeline
                 eaf eask editorconfig ef-themes eglot-java ejc-sql
                 elein embark-consult eshell-syntax-highlighting
                 evil-nerd-commenter ewal-doom-themes
                 exec-path-from-shell flx-ido flycheck-clojure
                 fringe-helper ghub git-commit gnu-elpa-keyring-update
                 gptel groovy-mode gruvbox-theme haskell-mode helm-lsp
                 helpful hideshow-org ibuffer-project
                 ibuffer-projectile ibuffer-vc immersive-translate
                 ivy-prescient json-mode json-navigator justify-kp
                 keycast lsp-ivy lsp-java lsp-ui magit magit-section
                 marginalia mct memoize nerd-icons-completion
                 nerd-icons-ibuffer nerd-icons-ivy-rich no-littering
                 nov orderless org-appear org-bullets org-contrib
                 org-noter org-preview-html org-protocol-jekyll
                 org-remark org-roam org-roam-ui org-ros
                 org-zettel-ref-mode ox-hugo paredit parent-mode
                 pinyinlib pkg-info popper promise rainbow-delimiters
                 restclient rg rime s shackle shrface spacemacs-theme
                 tagedit telega toc-org tree-sitter-langs ts-fold
                 vertico websocket which-key workgroups2 yasnippet
                 zotxt))
 '(package-vc-selected-packages
   '((s :url "https://github.com/magnars/s.el" :branch "master")
     (org-zettel-ref-mode :url
                          "https://github.com/yibie/org-zettel-ref-mode"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'scroll-left 'disabled nil)
