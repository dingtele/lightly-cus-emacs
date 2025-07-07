
(use-package go-translate
  ;; (setq gt-langs '(en fr))
  :custom  
  (gt-preset-translators
   `((ts-1 . ,(gt-translator
               :taker (gt-taker :langs '(en zh) :text 'buffer)
               :engines (list (gt-google-engine))
               ;; :render (gt-overlay-render)
               ;; :render (gt-buffer-render)
               :render (gt-overlay-render :type 'help-echo)
               ;; :render (gt-insert-render)
               )))))

;; bing-dict
(use-package bing-dict :ensure t)
(global-set-key (kbd "C-c d") 'bing-dict-brief)
(setq bing-dict-vocabulary-save t)
(setq bing-dict-vocabulary-file "~/Dropbox/vocabulary.org")

(defun capture-sentence-at-point ()
  "Capture the sentence where the word at point is located."
  (interactive)
  (let* ((word (thing-at-point 'word))  ; Get the word at point
         (sentence (save-excursion
                     (let ((sentence-start (progn
                                             (backward-sentence)  ; Move to the beginning of the sentence
                                             (point)))
                           (sentence-end (progn
                                           (forward-sentence)  ; Move to the end of the sentence
                                           (point))))
                       (message "000-sentence-start: %s\n111-sentence-end: %s\n" sentence-start sentence-end)
                       (buffer-substring-no-properties sentence-start sentence-end)))))  ; Get the sentence text
    (if word
        (message "The word is: %s\nThe sentence is: %s" word sentence)
      (message "No word found at point."))))

;; google-translate
(use-package google-translate
  :defines (google-translate-translation-directions-alist)
  :bind (("C-c g" . google-translate-smooth-translate))
  :config
  (setq google-translate-translation-directions-alist '(("en" . "zh-CN")))
  )




(use-package anki-helper
  :vc (:url "https://github.com/Elilif/emacs-anki-helper" :rev :newest)
  :custom
  (anki-helper-cloze-use-emphasis 'bold)
  (anki-helper-default-note-type "Cloze")
  (anki-helper-default-deck "saladict"))

;; A multi dictionaries interface
(use-package fanyi
  :ensure t
  ;; :bind (("C-c d f" . fanyi-dwim)
  ;;        ("C-c d d" . fanyi-dwim2)
  ;;        ("C-c d h" . fanyi-from-history))
  )



;; (use-package immersive-translate
;;   :ensure t
;;   :config
;;   (add-hook 'elfeed-show-mode-hook #'immersive-translate-setup)
;;   (add-hook 'nov-pre-html-render-hook #'immersive-translate-setup)
;;   )
;; (setq immersive-translate-backend 'DeepSeek
;;       immersive-translate-chatgpt-host "api.deepseek.com")

(provide 'init-dict)
