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

(setq gc-cons-threshold most-positive-fixnum)

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp" "customizations"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'.

Don't put large files in `site-lisp' directory, e.g. EAF.
Otherwise the startup will be very slow."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

(package-initialize)

(setq package-install-upgrade-built-in t)


(unless package-archive-contents
  (package-refresh-contents))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ad-redefinition-action 'accept)
 '(anki-helper-default-deck selected-deck)
 '(before-save-hook '(time-stamp font-lock-flush))
 '(blink-matching-paren-highlight-offscreen t)
 '(centaur-archives-package archives)
 '(confirm-kill-processes nil)
 '(custom-enabled-themes '(modus-operandi-tinted))
 '(custom-safe-themes
   '("5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874"
     "b41d0a9413fb0034cea34eb8c9f89f6e243bdd76bccecf8292eb1fefa42eaf0a"
     "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40"
     "6a5584ee8de384f2d8b1a1c30ed5b8af1d00adcbdcd70ba1967898c265878acf"
     "00d7122017db83578ef6fba39c131efdcb59910f0fac0defbe726da8072a0729" default))
 '(deft-extensions '("org"))
 '(kill-whole-line t)
 '(lisp-indent-function 'common-lisp-indent-function)
 '(load-prefer-newer t)
 '(mouse-yank-at-point t)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(org-agenda-files
   '("/Users/dingyu/Dropbox/org/2025.org"
     "/Users/dingyu/Dropbox/Journals/2025-06.org"))
 '(package-archives
   '(("ts-gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
     ("ts-melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
 '(package-selected-packages
   '(aggressive-indent all-the-icons-nerd-fonts anki-helper auctex auto-compile
                       auto-complete auto-dark auto-dim-other-buffers
                       auto-package-update auto-save beginend benchmark-init
                       bing-dict buffer-name-relative bufferlo bug-hunter burly
                       cal-china-x calibredb cape capf-autosuggest cdlatex
                       circadian clipetty cljsbuild-mode clojure-ts-mode
                       colorful-mode consult-flyspell consult-todo
                       consult-yasnippet counsel dash-functional datetime
                       default-text-scale deft denote diminish dimmer
                       dired-git-info dired-quick-sort dired-rsync diredfl
                       docker docker-compose-mode doom-modeline eask ef-themes
                       eglot-java ejc-sql eldoc-box elein elfeed elfeed-webkit
                       elisp-def elpy embark-consult eshell-syntax-highlighting
                       esup evil-nerd-commenter ewal-doom-themes
                       exec-path-from-shell fanyi fd-dired flx-ido
                       flycheck-clojure gcmh ghub gnu-elpa-keyring-update
                       go-translate goggles google-translate gptel gptel-quick
                       groovy-mode gruvbox-theme haskell-mode helm-lsp helpful
                       hideshow-org highlight-defined highlight-parentheses
                       hl-todo ibuffer-project ibuffer-projectile ibuffer-vc
                       iedit imenu-anywhere indent-bars inf-clojure
                       ivy-prescient json-mode json-navigator justify-kp keycast
                       lsp-brigde lsp-ivy lsp-java lsp-ui macrostep marginalia
                       mct memoize meow nano-minibuffer nano-modeline nano-theme
                       nerd-icons-completion nerd-icons-corfu nerd-icons-dired
                       nerd-icons-ibuffer nerd-icons-ivy-rich
                       netease-cloud-music no-littering nov nova ob-rust
                       orderless org-alert org-appear org-block-capf org-bullets
                       org-contrib org-journal org-make-toc org-modern org-node
                       org-noter org-preview-html org-protocol-jekyll org-remark
                       org-roam-ui org-ros org-sidebar org-supertag
                       org-zettel-ref-mode origami overseer ox-hugo
                       pangu-spacing paredit parent-mode pdf-tools pinyinlib
                       pkg-info popper pretty-symbols promise rainbow-delimiters
                       rainbow-identifiers reader region-occurrences-highlighter
                       rg rime rust-mode setup shackle shrface smartparens
                       smooth-scrolling sort-tab spacemacs-theme svg-tag-mode
                       tabspaces tagedit telega toc-org transient-posframe
                       transwin tree-sitter-langs treemacs-icons-dired
                       treemacs-magit treemacs-nerd-icons treemacs-persp
                       treemacs-perspective treemacs-projectile treemacs-tab-bar
                       treesit-auto valign vertico-posframe vundo workgroups2
                       xterm-color yasnippet-capf yasnippet-snippets zeft zotxt))
 '(package-vc-selected-packages
   '((anki-helper :url "https://github.com/Elilif/emacs-anki-helper")
     (reader :url "https://codeberg.org/divyaranjan/emacs-reader" :make "all")
     (sort-tab :url "https://github.com/manateelazycat/sort-tab")
     (pos-tag-highlight :url "https://github.com/yibie/pos-tag-highlight")
     (gptel-quick :url "https://github.com/karthink/gptel-quick")
     (auto-save :url "https://github.com/manateelazycat/auto-save")
     (zeft :url "https://github.com/casouri/zeft")
     (org-supertag :url "https://github.com/yibie/org-supertag")
     (lsp-brigde :url "https://github.com/manateelazycat/lsp-bridge")))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(show-trailing-whitespace nil)
 '(use-package-always-defer t)
 '(use-package-always-ensure t)
 '(use-package-compute-statistics t)
 '(use-package-verbose nil))

(use-package benchmark-init
  :ensure t
  :defer nil
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Set UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-next-selection-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq system-time-locale "C")

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


(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")
;; WORKAROUND: fix blank screen issue on macOS.
(defun fix-fullscreen-cocoa ()
  "Address blank screen issue with child-frame in fullscreen.
This issue has been addressed in 28."
  (if sys/mac-cocoa-p
      (bound-and-true-p ns-use-native-fullscreen)
    (setq ns-use-native-fullscreen nil)))

;; Misc
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))
(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil)     ; Permanently indent with spaces, never with TABs

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      ;; auto-save-default nil             ; Disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil
      word-wrap-by-category t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Maybe some code up here ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; (org-babel-tangle-file "~/.emacs.d/customizations/tools.org"
;;                        "~/.emacs.d/customizations/tools.el")


;; (setq gc-cons-threshold (* 2 1000 1000))


;; Customize
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-package)
(require 'init-core-overriding)
;; Preferences
(require 'init-base)
;; (require 'init-hydra)

(require 'init-ui)
(require 'init-edit)
(require 'init-completion)
(require 'init-snippet)

;; (require 'init-bookmark)
;; (require 'init-calendar)
;; (require 'init-dashboard)
(require 'init-dired)
(require 'init-highlight)
(require 'init-ibuffer)
;; (require 'init-kill-ring)
(require 'init-workspace)
(require 'init-window)
;; (require 'init-treemacs)

(require 'init-eshell)
;; (require 'init-shell)

;; (require 'init-markdown)
(require 'init-org)
(require 'init-reader)

(require 'init-ai)
(require 'init-dict)
;; (require 'init-docker)
;; (require 'init-player)

(require 'init-utils)

(require 'init-fold)
;; Programming
(require 'init-vcs)
;; (require 'init-check)
;; (require 'init-lsp)
;; (require 'init-dap)

(require 'init-progn)
(require 'init-elisp)

(load "~/.emacs.d/customizations/tools.el")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-default ((t (:inherit default)))))


(provide 'init)
