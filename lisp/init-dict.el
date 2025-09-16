;;; init-dict.el --- summary -*- lexical-binding: t -*-

;; Author: madcomet

;;; Commentary:

;;; Code:


(use-package gt
  :commands gt-translate
  :config
  (setq gt-default-translator
        (gt-translator
         :taker (gt-taker :langs '(en zh) :text 'paragraph :pick 'paragraph)
         :engines (list (gt-google-engine))
         :render (gt-overlay-render)))
  ;; :custom
  ;; (gt-preset-translators
  ;;  `((ts-1 . ,(gt-translator
  ;;              :taker (gt-taker :langs '(en zh) :text 'buffer)
  ;;              :engines (list (gt-google-engine))
  ;;              :render (gt-overlay-render)
  ;;              ;; :render (gt-buffer-render)
  ;;              ;; :render (gt-overlay-render :type 'help-echo)
  ;;              ;; :render (gt-insert-render)
  ;;              ))))
  )

;; bing-dict
(use-package bing-dict
  :ensure t
  :commands bing-dict-brief
  :bind ("C-c d" . bing-dict-brief)
  :custom
  (setq bing-dict-vocabulary-save t)
  (setq bing-dict-vocabulary-file "~/Dropbox/vocabulary.org"))


;; google-translate
(use-package google-translate
  :defines (google-translate-translation-directions-alist)
  :bind (("C-c g" . google-translate-smooth-translate))
  :config
  (setq google-translate-translation-directions-alist '(("en" . "zh-CN")))
  )


(use-package anki-helper
  ;; :vc (:url "https://github.com/Elilif/emacs-anki-helper" :rev :newest)
  :load-path "~/.emacs.d/site-lisp/anki-helper.el"
  :defer nil
  :commands +anki-helper-capture-cloze-card
  :custom
  (anki-helper-cloze-use-emphasis 'bold)
  (anki-helper-default-note-type "Cloze")
  (anki-helper-default-deck "saladict")
  :config
  (bind-key "C-c c c" '+anki-helper-capture-cloze-card)
  )

;; A multi dictionaries interface
(use-package fanyi
  :ensure t
  ;; :bind (("C-c d f" . fanyi-dwim)
  ;;        ("C-c d d" . fanyi-dwim2)
  ;;        ("C-c d h" . fanyi-from-history))
  )

(use-package org-anki
  :load-path "~/.emacs.d/site-lisp/org-anki/"
  :commands (org-anki-sync-entry org-anki-syn)
  :bind ("C-c b" . org-anki-sync-entry)
  :custom
  (org-anki-default-deck "Saladict"))

;; (use-package immersive-translate
;;   :ensure t
;;   :config
;;   (add-hook 'elfeed-show-mode-hook #'immersive-translate-setup)
;;   (add-hook 'nov-pre-html-render-hook #'immersive-translate-setup)
;;   )
;; (setq immersive-translate-backend 'DeepSeek
;;       immersive-translate-chatgpt-host "api.deepseek.com")

(provide 'init-dict)

;;; init-dict.el ends here
