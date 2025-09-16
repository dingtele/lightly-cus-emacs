;;; init-social.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package telega
  :commands (telega)
  :bind
  ("C-c t" . telega-hydra/body)
  :pretty-hydra
  ((:title (pretty-hydra-title "Telega" 'octicon "nf-oct-fold")
    :color amaranth :quit-key ("q" "C-g"))
   ("origin-map"
    (("a" telega-account-switch "switch account")
     ("b" telega-switch-buffer "switch buffer")
     ("c" telega-chat-with "chat with .."))
    "aditional"
    (("p" telega-chatbuf-filter-search "interactively search for messages in chat")
     ("d" telega-chat-remove-member "remove member from the chat")
     ("m" telega-describe-chat-members "show chat members")
     ("h" telega-notifications-history "show notification history")
     ("x" telega-chatbuf-thread-cancel "cancel filtering by chat"))))

  ;; :custom-face
  ;; (telega-msg-heading ((t (:inherit hl-line :background unspecified))))
  ;; (telega-msg-inline-reply ((t (:inherit (hl-line font-lock-function-name-face)))))
  ;; (telega-msg-inline-forward ((t (:inherit (hl-line font-lock-type-face)))))
  ;; (telega-msg-user-title ((t (:bold t))))
  :custom
  (telega-root-auto-fill-mode nil)
  (telega-chat-auto-fill-mode nil)
  (telega-emoji-use-images nil)
  (telega-notifications-mode 1)
  ;; (telega-msg-heading-with-date-and-status t) ; TODO
  ;; telega-debug t
  ;; telega-server-verbosity 4
  ;; adjust the size for sticker
  (telega-open-file-function 'org-open-file)
  (telega-chat-fill-column 90)
  (telega-sticker-size '(6 . 24))
  ;; 替代两行头像，防止头像因为字符高度不统一裂开。
  (telega-avatar-workaround-gaps-for '(return t))
  ;; 以下都是 telega-symbols-emojify 中的 telega-symbol
  ;; telega-symbol
  ;; remove iterm from `telega-symbols-emojify`
  (telega-symbols-emojify
   (cl-reduce (lambda (emojify key)
                (assq-delete-all key emojify))
              '(verified vertical-bar checkmark forum heavy-checkmark reply reply-quote horizontal-bar forward)
              :initial-value telega-symbols-emojify))
  (telega-symbol-verified (nerd-icons-codicon "nf-cod-verified_filled" :face 'telega-blue))
  (telega-symbol-vertical-bar "│") ;; U+2502 Box Drawings Light Vertical
  (telega-symbol-saved-messages-tag-end (nerd-icons-faicon "nf-fa-tag"))
  (telega-symbol-forum (nerd-icons-mdicon "nf-md-format_list_text"))
  (telega-symbol-flames (nerd-icons-mdicon "nf-md-delete_clock"))
  (telega-symbol-mark (propertize " " 'face 'telega-button-highlight))
  (telega-symbol-reply (nerd-icons-faicon "nf-fa-reply"))
  (telega-symbol-reply-quote (nerd-icons-faicon "nf-fa-reply_all"))
  (telega-symbol-forward (nerd-icons-faicon "nf-fa-mail_forward"))
  (telega-symbol-checkmark (nerd-icons-mdicon "nf-md-check"))
  (telega-symbol-heavy-checkmark (nerd-icons-codicon "nf-cod-check_all"))
  ;; palettes 根据使用主题的配色去置换
  (telega-builtin-palettes-alist '((light
                                    ((:outline "#b4637a") (:foreground "#b4637a"))
                                    ((:outline "#ea9d34") (:foreground "#ea9d34"))
                                    ((:outline "#907aa9") (:foreground "#907aa9"))
                                    ((:outline "#568D68") (:foreground "#568D68"))
                                    ((:outline "#286983") (:foreground "#286983"))
                                    ((:outline "#56949f") (:foreground "#56949f"))
                                    ((:outline "#d7827e") (:foreground "#d7827e")))
                                   (dark
                                    ((:outline "#eb6f92") (:foreground "#eb6f92"))
                                    ((:outline "#f6c177") (:foreground "#f6c177"))
                                    ((:outline "#b294bb") (:foreground "#b294bb"))
                                    ((:outline "#95b1ac") (:foreground "#95b1ac"))
                                    ((:outline "#81a2be") (:foreground "#81a2be"))
                                    ((:outline "#9ccfd8") (:foreground "#9ccfd8"))
                                    ((:outline "#ebbcba") (:foreground "#ebbcba")))))

  (telega-translate-to-language-by-default "zh")
  (telega-msg-save-dir "~/Downloads")
  (telega-chat-input-markups '("markdown2" "org"))
  (telega-autoplay-mode 1)
  (telega-url-shorten-regexps
   ;; telega-url-shorten
   (list `(too-long-link
           :regexp "^\\(https?://\\)\\(.\\{55\\}\\).*?$"
           :symbol ,(nerd-icons-faicon "nf-fa-link")
           :replace " \\1\\2...")))
  ;; telega-root
  (telega-root-default-view-function 'telega-view-folders)
  (telega-root-keep-cursor 'track)
  (telega-root-show-avatars nil)
  (telega-root-buffer-name "*Telega Root*")
  ;; remove chat folder icons
  (telega-chat-folders-insexp (lambda () nil))
  (telega-filters-custom nil)
  (telega-root-fill-column 150) ; fill-column
  (telega-filter-custom-show-folders nil)
  ;; :init
  ;; (setq telega-chat-folder-format nil)
  ;; :hook
  ;; ((telega-chat-mode . (lambda () (electric-pair-local-mode -1)))
  ;;  )
  :config
  (dolist (feature '(telega-url-shorten
                     telega-bridge-bot
                     telega-mnz
                     lib-telega
                     cl-lib
                     telega-notifications
                     ;; If language-detection is available,
                     ;; then laguage could be detected automatically
                     ;; for code blocks without language explicitly specified.
                     language-detection))
    (require feature nil t))
  (global-telega-url-shorten-mode 1)
  (global-telega-mnz-mode 1)

  )

(provide 'init-social)
;;; init-social.el ends here
