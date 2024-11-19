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
;; (setq default-directory "/home/jordi/")

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
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))


;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

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
(org-babel-tangle-file "~/.emacs.d/customizations/tools.org"
                       "~/.emacs.d/customizations/tools.el")
(load "~/.emacs.d/customizations/tools.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(ef-trio-light))
 '(custom-safe-themes
   '("9aa431bc3739422ffb91d9982b52d39cbf5fbe9b472fcdea3d6eccaafa65962f"))
 '(package-selected-packages
   '(a ace-jump-mode aggressive-indent anki-helper auto-complete
       auto-package-update bind-key bing-dict buffer-name-relative
       bug-hunter cal-china-x calibredb capf-autosuggest clipetty
       cljsbuild-mode clojure-ts-mode command-log-mode company corfu
       counsel datetime dimmer docker docker-compose-mode doom
       doom-modeline eask editorconfig ef-themes eglot-java ejc-sql
       elein embark-consult eshell-syntax-highlighting
       evil-nerd-commenter ewal-doom-themes exec-path-from-shell
       flx-ido flycheck-clojure ghub gnu-elpa-keyring-update gptel
       groovy-mode gruvbox-theme haskell-mode helm-lsp helpful
       hideshow-org hierarchy ibuffer-project ibuffer-projectile
       ibuffer-vc immersive-translate ivy-prescient json-mode
       json-navigator justify-kp keycast lsp-ivy lsp-java lsp-ui magit
       marginalia mct memoize nerd-icons-completion nerd-icons-ibuffer
       nerd-icons-ivy-rich netease-cloud-music no-littering nov
       orderless org-appear org-bullets org-contrib org-noter
       org-preview-html org-protocol-jekyll org-remark org-roam-ui
       org-ros org-zettel-ref-mode origami ox-hugo paredit parent-mode
       pdf-tools pinyinlib pkg-info popper promise rainbow-delimiters
       rainbow-identifiers restclient rg rime shackle shrface
       spacemacs-theme tagedit telega toc-org tree-sitter-langs
       ts-fold vertico workgroups2 yasnippet zotxt))
 '(package-vc-selected-packages
   '((org-zettel-ref-mode :url
                          "https://github.com/yibie/org-zettel-ref-mode"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

