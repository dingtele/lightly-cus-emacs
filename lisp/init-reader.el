(use-package org-zettel-ref-mode
  :ensure t
  :vc (:url "https://github.com/yibie/org-zettel-ref-mode" :rev :newest)
  ;; :load-path "~/.emacs.d/site-lisp/org-zettel-ref-mode/"
  :init
  (setq org-zettel-ref-overview-directory "~/Dropbox/org-roam-files/Notes/")
  :config
  ;; (setq org-zettel-ref-mode-type 'denote)
  (setq org-zettel-ref-mode-type 'org-roam)
  ;; (setq org-zettel-ref-mode-type 'normal)
  (setq org-zettel-ref-python-file "~/.emacs.d/elpa/org-zettel-ref-mode/convert-to-org.py")
  (setq org-zettel-ref-temp-folder "~/Dropbox/book-store/to-be-converted/")
  (setq org-zettel-ref-reference-folder "~/Dropbox/book-store/converted-org")
  (setq org-zettel-ref-archive-folder "~/Dropbox/book-store/archives/")
  (setq org-zettel-ref-python-environment 'venv)
  (setq org-zettel-ref-python-env-name "venv")
  (setq org-zettel-ref-debug t)
  (setq org-zettel-ref-highlight-types
        (append org-zettel-ref-highlight-types
                '(("warning" . (:char "w"
                                :face (:background "#FFA726"
                                       :foreground "#000000"
                                       :extend t)
                                :name "warning"
                                :prefix "⚠️"))
                  ("success" . (:char "s"
                                :face (:background "#66BB6A"
                                       :foreground "#FFFFFF"
                                       :extend t)
                                :name "success"
                                :prefix "✅")))))
  (define-key org-zettel-ref-mode-map (kbd "C-c q") 'org-zettel-ref-add-quick-note)
  (define-key org-zettel-ref-mode-map (kbd "C-c p") 'org-zettel-ref-quick-markup)
  )

(use-package mindstream
  :custom
  (mindstream-path "/home/madcomet/mindstream/anon")
  :config
  (mindstream-mode))

(use-package zeft
  :defer nil
  :vc (:url "https://github.com/casouri/zeft")
  :config
  (setq zeft-directory "~/Dropbox/Notes"))

(use-package deft
  :config
  (setq deft-directory "~/Dropbox/Notes")
  (setq deft-extensions '("org")))


(use-package ox-hugo
  :ensure t
  :defer t
  :after ox
  :custom
  (org-hugo-base-dir "~/codeBase/blogs/dingtele.github.io/"))

(use-package pos-tag-highlight
  :vc (:url "https://github.com/yibie/pos-tag-highlight" :rev :newest))

(with-eval-after-load 'nov
  (define-key nov-mode-map (kbd "<tab>") 'shrface-outline-cycle)
  (define-key nov-mode-map (kbd "S-<tab>") 'shrface-outline-cycle-buffer)
  (define-key nov-mode-map (kbd "C-t") 'shrface-toggle-bullets)
  (define-key nov-mode-map (kbd "C-j") 'shrface-next-headline)
  (define-key nov-mode-map (kbd "C-k") 'shrface-previous-headline)
  (define-key nov-mode-map (kbd "M-l") 'shrface-links-counsel) ; or 'shrface-links-helm or 'shrface-links-consult
  (define-key nov-mode-map (kbd "M-h") 'shrface-headline-consult)) ; or 'shrface-headline-helm or 'shrface-headline-consult

;; (setq package-vc-allow-build-commands t)
;; (use-package reader
;;   :vc (:url "https://codeberg.org/divyaranjan/emacs-reader"
;; 	    :make "all"))  


(use-package eww
  :hook (eww-mode . my-nov-font-setup))

(use-package mixed-pitch
  ;; :hook  (nov-mode .  mixed-pitch-mode)
  )
(use-package olivetti
  :defer nil
  :hook (nov-mode . olivetti-mode))

 ;;;;;; set for reading mode
;; (defun my-nov-font-setup ()
;;   (interactive)
;;   (face-remap-add-relative 'default
;;                            :family "times new roman"
;;                            :height 1.2)
;;   (face-remap-add-relative 'variable-pitch
;;                            :family "TsangerJinKai02"
;;                            :height 1.4))
;; (add-hook 'org-journal-mode-hook 'variable-pitch-mode)
;; (add-hook 'org-mode #'my-nov-font-setup)
;; (add-hook 'org-journal-mode-hook #'my-nov-font-setup)


;;epub reading
(use-package nov
  :defer nil
  :mode ("\\.epub\\'" . nov-mode)
  :bind (:map nov-mode-map
         ("j" . scroll-up-line)
         ("k" . scroll-down-line))
  :hook (nov-mode . olivetti-mode)
  :custom
  (nov-text-width 100)
  :init
  ;; (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  ;; (visual-line-mode 1)
  ;; (setq visual-fill-column-center-text t)
  ;; (add-hook 'nov-mode-hook 'visual-line-mode)
  ;; (add-hook 'nov-mode-hook 'visual-fill-column-mode)

  ;; (text-scale-set +1)
  ) 

;; (add-hook 'nov-mode-hook 'my-nov-font-setup)

;;Nov-rendering 
(use-package justify-kp
  :vc (:url "https://github.com/Fuco1/justify-kp" :rev latest-release) 
  :defer nil)

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

(require 'pdf-tools)
(pdf-tools-install)  ; Standard activation command
(pdf-loader-install) ; On demand loading, leads to faster startup time

;; == Markdown ==
(use-package markdown-mode
  :ensure t
  :defer t
  :init
  (add-hook 'markdown-mode-hook 'variable-pitch-mode)
  (add-hook 'markdown-mode-hook 'my-nov-font-setup)

  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :config
  (markdown-display-inline-images)

  )

(use-package flyspell
  :defer t
  :diminish (flyspell-mode . " φ"))

;;calibre
(use-package calibredb
  :ensure t
  :defer t
  :commands calibredb
  :bind ("\e\e b" . calibredb)
  :config
  (setq calibredb-root-dir "/Users/dingyu/Documents/calibre")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Books/books")
                                  ))
  )


;; Atom/RSS reader
(use-package elfeed
  ;; :pretty-hydra
  ;; ((:title (pretty-hydra-title "Elfeed" 'faicon "nf-fa-rss_square" :face 'nerd-icons-orange)
  ;;   :color amaranth :quit-key ("q" "C-g"))
  ;;  ("Search"
  ;;   (("c" elfeed-db-compact "compact db")
  ;;    ("g" elfeed-search-update--force "refresh")
  ;;    ("G" elfeed-search-fetch "update")
  ;;    ("y" elfeed-search-yank "copy URL")
  ;;    ("+" elfeed-search-tag-all "tag all")
  ;;    ("-" elfeed-search-untag-all "untag all"))
  ;;   "Filter"
  ;;   (("l" elfeed-search-live-filter "live filter")
  ;;    ("s" elfeed-search-set-filter "set filter")
  ;;    ("*" (elfeed-search-set-filter "@6-months-ago +star") "starred")
  ;;    ("a" (elfeed-search-set-filter "@6-months-ago") "all")
  ;;    ("t" (elfeed-search-set-filter "@1-day-ago") "today"))
  ;;   "Article"
  ;;   (("b" elfeed-search-browse-url "browse")
  ;;    ("n" next-line "next")
  ;;    ("p" previous-line "previous")
  ;;    ("u" elfeed-search-tag-all-unread "mark unread")
  ;;    ("r" elfeed-search-untag-all-unread "mark read")
  ;;    ("RET" elfeed-search-show-entry "show"))))
  :bind (("C-x w" . elfeed)
         :map elfeed-search-mode-map
         ("?" . elfeed-hydra/body)
         :map elfeed-show-mode-map
         ("q" . delete-window))
  ;; :hook (elfeed-show-mode . centaur-read-mode)
  :init (setq url-queue-timeout 30
              elfeed-db-directory (locate-user-emacs-file ".elfeed")
              elfeed-show-entry-switch #'pop-to-buffer
              elfeed-show-entry-delete #'delete-window
              ;; elfeed-feeds '(("https://planet.emacslife.com/atom.xml" planet emacslife)
              ;;                ("http://www.masteringemacs.org/feed/" mastering)
              ;;                ("https://oremacs.com/atom.xml" oremacs)
              ;;                ("https://pinecast.com/feed/emacscast" emacscast)
              ;;                ("https://emacstil.com/feed.xml" Emacs TIL)
              ;;                ;; ("https://www.reddit.com/r/emacs.rss" reddit))
              )
  :config
  ;; Ignore db directory in recentf
  (push elfeed-db-directory recentf-exclude)

  ;; Add icons via tags

  (defun nerd-icon-for-tags (tags)
    "Generate Nerd Font icon based on tags.
Returns default if no match."
    (cond ((member "youtube" tags)  (nerd-icons-faicon "nf-fa-youtube_play" :face '(:foreground "#FF0200")))
          ((member "instagram" tags) (nerd-icons-faicon "nf-fa-instagram" :face '(:foreground "#FF00B9")))
          ((or (member "emacs" tags) (member "emacslife" tags) (member "mastering" tags))
           (nerd-icons-sucicon "nf-custom-emacs" :face '(:foreground "#9A5BBE")))
          ((member "github" tags) (nerd-icons-faicon "nf-fa-github"))
          (t (nerd-icons-faicon "nf-fae-feedly" :face '(:foreground "#2AB24C")))))

  (defun lucius/elfeed-search-print-entry--better-default (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (date-width (car (cdr elfeed-search-date-format)))
           (title (concat (or (elfeed-meta entry :title)
                              (elfeed-entry-title entry) "")
                          ;; NOTE: insert " " for overlay to swallow
                          " "))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (mapconcat (lambda (s) (propertize s 'face 'elfeed-search-tag-face)) tags ","))
           (title-width (- (frame-width)
                           ;; (window-width (get-buffer-window (elfeed-search-buffer) t))
                           date-width elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title (elfeed-clamp
                                 elfeed-search-title-min-width
                                 title-width
                                 elfeed-search-title-max-width) :left))

           ;; Title/Feed ALIGNMENT
           (align-to-feed-pixel (+ date-width
                                   (max elfeed-search-title-min-width
                                        (min title-width elfeed-search-title-max-width)))))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize title-column 'face title-faces 'kbd-help title))
      (put-text-property (1- (point)) (point) 'display `(space :align-to ,align-to-feed-pixel))
      ;; (when feed-title (insert " " (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when feed-title
        (insert " " (concat (nerd-icon-for-tags tags) " ")
                (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when tags (insert "(" tags-str ")"))))

  (setq  elfeed-search-print-entry-function #'lucius/elfeed-search-print-entry--better-default)

  ;; Use xwidget if possible
  (with-no-warnings
    (defun my-elfeed-show-visit (&optional use-generic-p)
      "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'."
      (interactive "P")
      (let ((link (elfeed-entry-link elfeed-show-entry)))
        (when link
          (message "Sent to browser: %s" link)
          (if use-generic-p
              (browse-url-generic link)
            (centaur-browse-url link)))))
    (advice-add #'elfeed-show-visit :override #'my-elfeed-show-visit)

    (defun my-elfeed-search-browse-url (&optional use-generic-p)
      "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'."
      (interactive "P")
      (let ((entries (elfeed-search-selected)))
        (cl-loop for entry in entries
                 do (elfeed-untag entry 'unread)
                 when (elfeed-entry-link entry)
                 do (if use-generic-p
                        (browse-url-generic it)
                      (centaur-browse-url it)))
        (mapc #'elfeed-search-update-entry entries)
        (unless (or elfeed-search-remain-on-entry (use-region-p))
          (forward-line))))
    (advice-add #'elfeed-search-browse-url :override #'my-elfeed-search-browse-url)))

;; (use-package elfeed-webkit
;;   :ensure
;;   :after elfeed)

;; Another Atom/RSS reader
(use-package newsticker
  :ensure nil
  :bind ("C-x W" . newsticker-show-news)
  ;; :hook (newsticker-treeview-item-mode . centaur-read-mode)
  :init (setq newsticker-url-list
              '(("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
                ("Mastering Emacs" "http://www.masteringemacs.org/feed/")
                ("Oremacs" "https://oremacs.com/atom.xml")
                ("EmacsCast" "https://pinecast.com/feed/emacscast")
                ("Emacs TIL" "https://emacstil.com/feed.xml")
                ;; ("Emacs Reddit" "https://www.reddit.com/r/emacs.rss")
                )))

(provide 'init-reader)
