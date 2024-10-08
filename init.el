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

(require 'ui)
(require 'editing)
(require 'shell-integration)
(require 'navigation)
(require 'misc)
(require 'init-site-lisp)
(require 'init-core-overriding)
;; Langauage-specific

;; (require 'elisp-editing)
(require 'init-org)
(require 'tools)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-timeout-seconds 2.0)
 '(coffee-tab-width 2)
 '(custom-enabled-themes '(nord))
 '(custom-safe-themes
   '("de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0" "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" "bbb13492a15c3258f29c21d251da1e62f1abb8bbd492386a673dcfab474186af" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "9ed206ff6874db89cb4a588c6cdc75a7b056fecbc9880e9758881bdef6d9d79a" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "3b5bac2bef0c51a169be7e9b5f80414e662e5eb2e3e3cf126873325e9344d26e" "3cf1845f34a1180b390a94e7cef30912f60c152c520e03d08868cc7b70aaa9c8" "6631f884f5f43e9d8eee42f5bcf8522a7f791688d2d2667ec135c129066be243" "c2c63381042e2e4686166d5d59a09118017b39003e58732b31737deeed454f1c" "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0" "4c7228157ba3a48c288ad8ef83c490b94cb29ef01236205e360c2c4db200bb18" "b73a23e836b3122637563ad37ae8c7533121c2ac2c8f7c87b381dd7322714cd0" "4871b9580169db848da98ba561259089fd83cbbe7b12481db6ca2d906a844154" "95d5336ac1ba49c76ea64028945cc72dfaf98dde7edea58aca4f11874d3191ca" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "79a8c85692a05a0ce0502168bb0e00d25f021a75d8b0136b46978bddf25e3b72" "84b04a13facae7bf10f6f1e7b8526a83ca7ada36913d1a2d14902e35de4d146f" "2ca3da7d36b0d326f984530a07be54b272b5c313b1361989acf747d8b5616162" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" default))
 '(desktop-path '("/home/madcomet/.emacs.d/var/desktop/"))
 '(highlight-indent-guides-auto-character-face-perc 20)
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-indent-guides-auto-even-face-perc 15)
 '(highlight-indent-guides-auto-odd-face-perc 15)
 '(highlight-indent-guides-character '|)
 '(highlight-indent-guides-method 'character)
 '(package-selected-packages
   '(desktop-recover nano-theme doom-modeline-now-playing nano-modeline spacemacs-theme gptel google-translate org-anki org-drill rime capf-autosuggest sicp corg dimmer all-the-icons-nerd-fonts all-the-icons-gnus eshell-syntax-highlighting org-roam emacsql-sqlite all-the-icons treemacs-tab-bar treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil nord-theme one-themes highlight-indent-guides treemacs org-modern org-contrib hydra helpful ivy-prescient counsel which-key command-log-mode no-littering auto-package-update restclient docker-compose-mode docker telega marginalia orderless vertico keycast json-navigator json-mode doom ewal-doom-themes corfu magit tagedit rainbow-delimiters cider exec-path-from-shell))
 '(safe-local-variable-values
   '((vc-default-patch-addressee . "bug-gnu-emacs@gnu.org")
     (etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc")
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/" "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))))
 '(show-paren-style 'expression)
 '(telega-server-libs-prefix "/usr/local")
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

