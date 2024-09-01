(use-package eaf
  :load-path "~/codebase/emacs-application-framework"
  :custom
  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (defalias 'browse-web #'eaf-open-browser)
  ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  (require 'eaf-browser)

) ;; unbind, see more in the Wiki

(require 'eaf-browser)
(require 'eaf-pdf-viewer)
(require 'eaf-image-viewer)
(require 'eaf-rss-reader)
(require 'eaf-terminal)
(require 'eaf-markdown-previewer)
(require 'eaf-org-previewer)
(require 'eaf-git)
(require 'eaf-file-manager)
(require 'eaf-mindmap)
(require 'eaf-netease-cloud-music)
(require 'eaf-system-monitor)
(require 'eaf-file-browser)
(require 'eaf-file-sender)
(require 'eaf-airshare)
(require 'eaf-jupyter)
(require 'eaf-markmap)
(require 'eaf-demo)
(require 'eaf-vue-demo)
(require 'eaf-vue-tailwindcss)
(require 'eaf-pyqterminal)

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

;;eshell
(use-package eshell
   :config
   (setq eshell-scroll-to-bottom-on-input t)
   (setq-local tab-always-indent 'complete)
   (setq eshell-history-size 10000)
   (setq eshell-save-history-on-exit t) ;; Enable history saving on exit
   (setq eshell-hist-ignoredups t) ;; Ignore duplicates
   :hook
   (eshell-mode . my/eshell-hook))

(use-package capf-autosuggest
   :hook
   (eshell-mode . capf-autosuggest-mode))

(defun my/shell-create (name)
   "Create a custom-named eshell buffer with NAME."
   (interactive "sName: ")
   (eshell 'new)
   (let ((new-buffer-name (concat "*eshell-" name "*")))
     (rename-buffer new-buffer-name t)))

(global-set-key (kbd "M-o s") #'my/shell-create)

(use-package popper
   :init
   (setq popper-reference-buffers
     '("\\*eshell.*"
        flymake-diagnostics-buffer-mode
        help-mode
        compilation-mode))
   (popper-mode 1)
   (popper-echo-mode 1)
   :custom
   (popper-window-height 15))

(bind-key* (kbd "C-;") #'popper-toggle)
