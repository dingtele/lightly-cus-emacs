;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

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
                         ("org" . "https://orgmode.org/elpa/")
			  ("melpa" . "https://melpa.org/packages/")
                          ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

                         

(package-initialize)


(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)




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

;;epub reading
(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :bind (:map nov-mode-map
              ("j" . scroll-up-line)
              ("k" . scroll-down-line))
  )
;;calibre
(use-package calibredb
  :ensure t
  :commands calibredb
  :bind ("\e\e b" . calibredb)
  :config
  (setq calibredb-root-dir "/Users/dingyu/Documents/calibre")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Books/books")
                                  ))
  )

;; bing-dict
(use-package bing-dict :ensure t)
(global-set-key (kbd "C-c d") 'bing-dict-brief)
(setq bing-dict-vocabulary-save t)
;;
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

;; (use-package eaf
;;   :load-path "~/emacs-application-framework"
;;   :custom
;;   ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser)
;;   :config
;;   (defalias 'browse-web #'eaf-open-browser)
;;   ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key nil "M-q" eaf-browser-keybinding)
;;   (require 'eaf-browser)

;; ) ;; unbind, see more in the Wiki
;; Hard-to-categorize customizations
;(require 'misc)

;; Langauage-specific
;; (require 'elisp-editing)

(require 'init-org)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-timeout-seconds 2.0)
 '(coffee-tab-width 2)
 '(custom-enabled-themes '(ef-cyprus))
 '(custom-safe-themes
   '("4871b9580169db848da98ba561259089fd83cbbe7b12481db6ca2d906a844154" "95d5336ac1ba49c76ea64028945cc72dfaf98dde7edea58aca4f11874d3191ca" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "79a8c85692a05a0ce0502168bb0e00d25f021a75d8b0136b46978bddf25e3b72" "84b04a13facae7bf10f6f1e7b8526a83ca7ada36913d1a2d14902e35de4d146f" "2ca3da7d36b0d326f984530a07be54b272b5c313b1361989acf747d8b5616162" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" default))
 '(highlight-indent-guides-auto-character-face-perc 20)
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-indent-guides-auto-even-face-perc 15)
 '(highlight-indent-guides-auto-odd-face-perc 15)
 '(highlight-indent-guides-character '|)
 '(highlight-indent-guides-method 'character)
 '(package-selected-packages
   '(highlight-indent-guides treemacs org-modern org-contrib hydra helpful ivy-prescient counsel which-key command-log-mode no-littering auto-package-update restclient docker-compose-mode docker telega marginalia orderless vertico keycast json-navigator json-mode doom ewal-doom-themes corfu magit tagedit rainbow-delimiters cider clojure-mode-extra-font-locking clojure-mode exec-path-from-shell))
 '(show-paren-style 'expression)
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

