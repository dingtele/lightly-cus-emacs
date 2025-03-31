;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; global keyword to turn on / off:
;; TODO: review the operation in detail when it is needed
;; PERF: operations that is possible to cause performance issue, modify accordingly


;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)
(setq debug-on-quit nil)
;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; ;; ignore native compile warning
(setq warning-minimum-level :emergency)
;; (setq default-directory "/home/jordi/")

;; PERF
;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

(setq gc-cons-threshold most-positive-fixnum)

(defconst *IS-MAC* (eq system-type 'darwin))
(defconst *IS-LINUX* (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst *IS-WINDOWS*  (memq system-type '(cygwin windows-nt ms-dos)))

(defun display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Maybe some code up here ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org)

;; (defun zz/org-babel-tangle-async (file)
;;   "Invoke `org-babel-tangle-file' asynchronously."
;;   (message "Tangling %s..." (buffer-file-name))
;;   (async-start
;;    (let ((args (list file)))
;;      `(lambda ()
;;         (require 'org)
;;         ;;(load "~/.emacs.d/init.el")
;;         (let ((start-time (current-time)))
;;           (apply #'org-babel-tangle-file ', args)
;;           (format "%.2f" (float-time (time-since start-time))))))
;;    (let ((message-string (format "Tangling %S completed after " file)))
;;      `(lambda (tangle-time)
;;         (message (concat ,message-string
;;                          (format "%s seconds" tangle-time)))))))

;; (zz/org-babel-tangle-async "~/.emacs.d/customizations/tools.org")

(org-babel-tangle-file "~/.emacs.d/customizations/tools.org"
                       "~/.emacs.d/customizations/tools.el")

(load "~/.emacs.d/customizations/tools.el")

(setq gc-cons-threshold (* 2 1000 1000))


;; Customize


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ad-redefinition-action 'accept)
 '(anki-helper-cloze-use-emphasis 'verbatim)
 '(anki-helper-default-deck "org-deck")
 '(anki-helper-default-note-type "Cloze")
 '(blink-matching-paren-highlight-offscreen t)
 '(confirm-kill-processes nil)
 '(custom-enabled-themes '(doom-feather-light))
 '(custom-safe-themes
   '("e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40"
     "6a5584ee8de384f2d8b1a1c30ed5b8af1d00adcbdcd70ba1967898c265878acf"
     "00d7122017db83578ef6fba39c131efdcb59910f0fac0defbe726da8072a0729"
     default))
 '(deft-extensions '("org"))
 '(kill-whole-line t)
 '(load-prefer-newer t t)
 '(mouse-yank-at-point t)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(org-agenda-files '("/Users/dingyu/Dropbox/org/2025.org"))
 '(package-archives
   '(("ts-gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
     ("ts-melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
 '(package-selected-packages
   '(a aggressive-indent all-the-icons-nerd-fonts auctex auto-compile
       auto-complete auto-package-update bing-dict
       buffer-name-relative bug-hunter burly cal-china-x calibredb
       cape capf-autosuggest cdlatex clipetty cljsbuild-mode
       clojure-ts-mode colorful-mode consult-todo counsel
       dash-functional datetime deft denote dimmer docker
       docker-compose-mode doom-modeline eask ef-themes eglot-java
       ejc-sql el-job eldoc-box elein elisp-def elpy embark-consult
       eshell-syntax-highlighting esup evil-nerd-commenter
       evil-smartparens ewal-doom-themes exec-path-from-shell flx-ido
       flycheck-clojure gcmh ghub gnu-elpa-keyring-update go-translate
       gptel groovy-mode gruvbox-theme haskell-mode helm-lsp helpful
       hideshow-org ibuffer-project ibuffer-projectile ibuffer-vc
       iedit imenu-anywhere inf-clojure ivy-prescient json-mode
       json-navigator justify-kp keycast lsp-ivy lsp-java lsp-ui magit
       marginalia mct memoize meow nano-theme nerd-icons-completion
       nerd-icons-corfu nerd-icons-ibuffer nerd-icons-ivy-rich
       netease-cloud-music no-littering nov nova ob-rust orderless
       org-alert org-anki org-appear org-block-capf org-bullets
       org-contrib org-journal org-make-toc org-modern org-node
       org-noter org-preview-html org-protocol-jekyll org-remark
       org-roam org-roam-ui org-ros org-sidebar org-zettel-ref-mode
       origami ox-hugo paredit parent-mode pdf-tools pinyinlib
       pkg-info popper rainbow-delimiters rainbow-identifiers
       region-occurrences-highlighter rg rime rust-mode setup shackle
       shrface smartparens smooth-scrolling spacemacs-theme svg-lib
       svg-tag-mode tabspaces tagedit telega toc-org
       transient-posframe tree-sitter-langs treesit-auto
       vertico-posframe workgroups2 xterm-color zeft zotxt))
 '(package-vc-selected-packages
   '((zeft :url "https://github.com/casouri/zeft")
     (org-supertag :url "https://github.com/yibie/org-supertag")
     (lsp-brigde :url "https://github.com/manateelazycat/lsp-bridge")))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(show-trailing-whitespace nil)
 '(use-package-always-defer t)
 '(use-package-always-ensure t)
 '(use-package-verbose nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(provide 'init)
