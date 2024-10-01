(use-package rime
  :custom
  (default-input-method "rime")
  :config
  (setq rime-translate-keybindings
        '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>"))
  (setq rime-user-data-dir "~/.emacs.d/var/rime/")
  ;; (setq rime-share-data-dir "~/.local/share/fcitx5/rime/")
  (setq rime-share-data-dir "/usr/share/rime-data/")
  (setq rime-disable-predicates
        '(rime-predicate-after-alphabet-char-p
          rime-predicate-current-input-punctuation-p
          rime-predicate-current-uppercase-letter-p
          rime-predicate-punctuation-line-begin-p
          rime--prog-in-code-p))
  (setq rime-cursor "˰")
  (setq rime-inline-predicates
        ;; If cursor beis after a whitespace
        ;; which follow a non-ascii character.
        '(rime-predicate-space-after-cc-p))
;;; support shift-l, shift-r, control-l, control-r
  (setq rime-inline-ascii-trigger 'shift-r)
  (defun rime-predicate-special-ascii-line-begin-p ()
    "If '/' or '#' at the beginning of the line."
    (and (> (point) (save-excursion (back-to-indentation) (point)))
         (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80))))))
           (string-match-p "^[\/#]" string)))
)

;; Restore Opened Files
(progn
  ;; (desktop-save-mode 1)
  ;; ;; save when quit
  ;; (setq desktop-save t)

  ;; ;; no ask if crashed
  ;; (setq desktop-load-locked-desktop t)

  ;; (setq desktop-restore-frames t)

  ;; (setq desktop-auto-save-timeout 300)

  ;; ;; save some global vars
  ;; (setq desktop-globals-to-save nil)
  ;; ;; 2023-09-16 default
  ;; ;; '(desktop-missing-file-warning tags-file-name tags-table-list search-ring regexp-search-ring register-alist file-name-history)
  ;; (setq desktop-dirname "~/.emacs.d/var/desktop/")
)

(progn
;; (require ' desktop-recover)
;;  ;; optionallly:
;; (setq desktop-recover-location
;;     (desktop-recover-fixdir "~/.emacs.d/var/desktop/")) 
;;  ;; Brings up the interactive buffer restore menu
;; (desktop-recover-interactive)
 ;; Note that after using this menu, your desktop will be saved
 ;; automatically (triggered by the auto-save mechanism).
 ;; For finer-grained control of the frequency of desktop saves,
 ;; you can add the standard keybindings to your set-up:
 ;;  (desktop-recover-define-global-key-bindings "\C-c%")
 )

(require 'workgroups)
(setq wg-prefix-key (kbd "C-c w"))
(workgroups-mode 1)
(wg-load "~/.emacs.d/var/workgroups")

(require 'layout-restore)

(use-package gptel
  :ensure t
  :config
  ;; default backend configuration
  (setq
   gptel-model "codegeex4:latest"
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '("codegeex4:latest")))
  ;; ;; DeepSeek offers an OpenAI compatible API
  ;; (setq gptel-model   "deepseek-chat"
  ;;       gptel-backend
  ;;       (gptel-make-openai "DeepSeek"     ;Any name you want
  ;;         :host "api.deepseek.com"
  ;;         :endpoint "/chat/completions"
  ;;         :stream t
  ;;         :key (get-openai-api-key)             ;can be a function that returns the key
  ;;         :models '("deepseek-chat" "deepseek-coder")))

  ;; (defun get-openai-api-key ()
  ;; "Return the OpenAI API key from ~/.authinfo."
  ;; (let ((authinfo-file (expand-file-name "~/.authinfo")))
  ;;   (with-temp-buffer
  ;;     (insert-file-contents authinfo-file)
  ;;     (goto-char (point-min))
  ;;     (when (re-search-forward "^machine api\\.deepseek\\.com login apikey password \\(\\S-+\\)$" nil t)
  ;;       (match-string 1)))))
)

(require 'immersive-translate)
(add-hook 'elfeed-show-mode-hook #'immersive-translate-setup)
(add-hook 'nov-pre-html-render-hook #'immersive-translate-setup)

(use-package magit
  :ensure t
  :hook (git-commit-mode . flyspell-mode)
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :custom
  (magit-diff-refine-hunk t)
  (magit-ediff-dwim-show-on-hunks t)
)

(use-package ts-fold
  :vc (:fetcher github :repo "emacs-tree-sitter/ts-fold"))

;; (use-package eaf
;;   :load-path "~/codebase/emacs-application-framework"
;;   :custom
;;   ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser)
;;   :config
;;   (defalias 'browse-web #'eaf-open-browser)
;;   ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   ;; (eaf-bind-key nil "M-q" eaf-browser-keybinding)
;;   (require 'eaf-browser)

;; ) ;; unbind, see more in the Wiki

;; (require 'eaf-browser)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-image-viewer)
;; (require 'eaf-rss-reader)
;; (require 'eaf-terminal)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-org-previewer)
;; (require 'eaf-git)
;; (require 'eaf-file-manager)
;; (require 'eaf-mindmap)
;; (require 'eaf-netease-cloud-music)
;; (require 'eaf-system-monitor)
;; (require 'eaf-file-browser)
;; (require 'eaf-file-sender)
;; (require 'eaf-airshare)
;; (require 'eaf-jupyter)
;; (require 'eaf-markmap)
;; (require 'eaf-demo)
;; (require 'eaf-vue-demo)
;; (require 'eaf-vue-tailwindcss)
;; (require 'eaf-pyqterminal)

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

;; google-translate
(use-package google-translate
  :defines (google-translate-translation-directions-alist)
  :bind (("C-c g" . google-translate-smooth-translate))
  :config
  (setq google-translate-translation-directions-alist '(("en" . "zh-CN")))
)

;; eshell
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

;; (global-set-key (kbd "/M-o s") #'my/shell-create)

(use-package popper
   :init
   (setq popper-reference-buffers
     '("\\*eshell.*"
        flymaxxke-diagnostics-buffer-mode
        help-mode
        compilation-mode))
   (popper-mode 1)
   (popper-echo-mode 1)
   :custom
   (popper-window-height 15))

(bind-key* (kbd "C-;") #'popper-toggle)

;; 将原本放在 .emacs.d 目录下的一些配置信息或动态信息，转移到 etc 或 var 子目录里，让配置目录更加简洁清爽
(use-package no-littering
  :ensure t)

(provide 'tools)
