;; Restore Opened Files
(progn
  (desktop-save-mode 1)
  ;; save when quit
  (setq desktop-save t)

  ;; no ask if crashed
  (setq desktop-load-locked-desktop t)
  (setq desktop-restore-frames t)
  (setq desktop-auto-save-timeout 300)

  ;; save some global vars
  (setq desktop-globals-to-save nil)
  ;; 2023-09-16 default
  ;; '(desktop-missing-file-warning tags-file-name tags-table-list search-ring regexp-search-ring register-alist file-name-history)
  (setq desktop-dirname "~/.emacs.d/var/desktop/")
)

;; (progn
;;   (require ' desktop-recover)
;;   ;; optionallly:
;;   (setq desktop-recover-location
;;         (desktop-recover-fixdir "~/.emacs.d/var/desktop/")) 
;;   ;; Brings up the interactive buffer restore menu
;;   (desktop-recover-interactive)
;;   ;; Note that after using this menu, your desktop will be saved
;;   ;; automatically (triggered by the auto-save mechanism).
;;   ;; For finer-grained control of the frequency of desktop saves,
;;   ;; you can add the standard keybindings to your set-up:
;;   (desktop-recover-define-global-key-bindings "\C-c%")
;; )

(use-package workgroups2
  :init (setq wg-prefix-key (kbd "C-c w"))
  :config
  (workgroups-mode 1)
  (setq wg-session-file "~/.emacs.d/var/workgroups")
)

(add-to-list 'load-path "~/.emacs.d/site-lisp/copilot.el-main")
(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(use-package gptel
  :ensure t
  :config
  ;; default backend configuration
  ;; (setq
  ;;  gptel-model "codegeex4:latest"
  ;;  gptel-backend (gptel-make-ollama "Ollama"
  ;;                  :host "localhost:11434"
  ;;                  :stream t
  ;;                  :models '("codegeex4:latest")))

  ;; DeepSeek offers an OpenAI compatible API
  (defun get-openai-api-key ()
    "Return the OpenAI API key from ~/.authinfo."
    (let ((authinfo-file (expand-file-name "~/.authinfo")))
      (with-temp-buffer
        (insert-file-contents authinfo-file)
        (goto-char (point-min))
        (when (re-search-forward "^machine api\\.deepseek\\.com login apikey password \\(\\S-+\\)$" nil t)
          (match-string 1)))))

  (setq gptel-model   "deepseek-chat"
        gptel-backend
        (gptel-make-openai "DeepSeek"     ;Any name you want
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key (get-openai-api-key)             ;can be a function that returns the key
          :models '("deepseek-chat" "deepseek-coder")))

  )

(use-package immersive-translate
  :ensure t
  :config
  (add-hook 'elfeed-show-mode-hook #'immersive-translate-setup)
  (add-hook 'nov-pre-html-render-hook #'immersive-translate-setup)
  )
(setq immersive-translate-backend 'DeepSeek
      immersive-translate-chatgpt-host "api.deepseek.com")

(use-package ox-hugo
  :ensure t
  :after ox)

(require 'ejc-sql)
(setq clomacs-httpd-default-port 8090) ; Use a port other than 8080.
;; Require completion frontend (autocomplete or company). One of them or both.
(require 'ejc-autocomplete)
(add-hook 'ejc-sql-minor-mode-hook
          (lambda ()
            (auto-complete-mode t)
            (ejc-ac-setup)))

(setq ejc-use-flx t)
(setq ejc-flx-threshold 2)
(require 'ejc-company)
(push 'ejc-company-backend company-backends)
(add-hook 'ejc-sql-minor-mode-hook
          (lambda ()
            (company-mode t)))
(setq ejc-complete-on-dot t)
;; (company-quickhelp-mode t)
(setq ejc-completion-system 'standard)

(add-hook 'ejc-sql-minor-mode-hook
          (lambda ()
            (ejc-eldoc-setup)))
;; Performance & output customization
(add-hook 'ejc-sql-connected-hook
          (lambda ()
            (ejc-set-fetch-size 50)
            (ejc-set-max-rows 50)
            (ejc-set-show-too-many-rows-message t)
            (ejc-set-column-width-limit 25)
            (ejc-set-use-unicode t)))
(setq ejc-result-table-impl 'ejc-result-mode)
;; PostgreSQL example
(ejc-create-connection
 "PostgreSQL-db-connection"
 :classpath (concat "~/.m2/repository/org.postgresql/postgresql/42.6.0/"
                    "postgresql-42.6.0.jar")
 :subprotocol "postgresql"
 :subname "//aws06mlicdevpsql01.aws06.mlic.cloud:5432/mli_qaa01_v20"
 :user "mli_qaa01_v20"
 :password "mli_qaa01_v20")

(require 'init-treesitter)

;;epub reading
(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :bind (:map nov-mode-map
              ("j" . scroll-up-line)
              ("k" . scroll-down-line)))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq nov-text-width 80)
(setq nov-text-width t)
(setq visual-fill-column-center-text t)
(add-hook 'nov-mode-hook 'visual-line-mode)
(add-hook 'nov-mode-hook 'visual-fill-column-mode)
;;nov-rendering
(use-package justify-kp
  :ensure t
  :vc (:fetcher github :repo "Fuco1/justify-kp"))
(setq nov-text-width t)

(defun my-nov-window-configuration-change-hook ()
  (my-nov-post-html-render-hook)
  (remove-hook 'window-configuration-change-hook
               'my-nov-window-configuration-change-hook
               t))
(defun my-nov-post-html-render-hook ()
  (if (get-buffer-window)
      (let ((max-width (pj-line-width))
            buffer-read-only)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (not (looking-at "^[[:space:]]*$"))
              (goto-char (line-end-position))
              (when (> (shr-pixel-column) max-width)
                (goto-char (line-beginning-position))
                (pj-justify)))
            (forward-line 1))))
    (add-hook 'window-configuration-change-hook
              'my-nov-window-configuration-change-hook
              nil t)))

(add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook)

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
(setq bing-dict-vocabulary-file "~/Dropbox/vocabulary.org")

;; google-translate
;; (use-package google-translate
;;   :defines (google-translate-translation-directions-alist)
;;   :bind (("C-c g" . google-translate-smooth-translate))
;;   :config
;;   (setq google-translate-translation-directions-alist '(("en" . "zh-CN")))
;; )

(use-package rg)

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
