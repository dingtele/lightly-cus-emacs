(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :defer nil
  :init
  (defun +tab-bar-tab-name-function ()
    "Generate a name for the current tab based on the buffer name.
        If the buffer name exceeds `tab-bar-tab-name-truncated-max` characters,
        truncate it and append `tab-bar-tab-name-ellipsis`.  If there are multiple
        windows in the tab, append the count of windows in parentheses.
        Return the formatted tab name."
    (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
           (count (length (window-list-1 nil 'nomini)))
           (truncated-tab-name (if (< (length raw-tab-name)
                                      tab-bar-tab-name-truncated-max)
                                   raw-tab-name
                                 (truncate-string-to-width raw-tab-name
                                                           tab-bar-tab-name-truncated-max
                                                           nil nil tab-bar-tab-name-ellipsis))))
      (if (> count 1)
          (concat truncated-tab-name "(" (number-to-string count) ")")
        truncated-tab-name)))

  (defun +tab-bar-tab-name-format-function (tab i)
    "Format the display name for a tab in the tab bar.
        TAB is the tab descriptor, and I is the tab index.  Apply custom
        styling to the tab name and index using `tab-bar-tab-face-function`.

        - Prefix the tab with its index and a colon, styled with a bold weight.
        - Surround the tab name with spaces, adjusting vertical alignment
          for aesthetics.
        - Return the formatted tab name with applied text properties."
    (let ((face (funcall tab-bar-tab-face-function tab)))
      (concat
       ;; change tab-bar's height
       (propertize " " 'display '(raise 0.25))
       (propertize (format "%d:" i) 'face `(:inherit ,face :weight ultra-bold))
       (concat " " (propertize (alist-get 'name tab) 'face `(:inherit ,face :underline t)) " ")
       (propertize " " 'display '(raise -0.25))
       )))
  ;; :config
  ;; (require 'lib-svg-tag-mode)
  ;; (add-hook 'tab-bar-new-tab 'lib-svg-tag-mode)
  :bind (("s-t" . tab-bar-new-tab)
         ("s-w" . (lambda() (interactive)
                    (if (y-or-n-p "really want to close this tab?")
                        (tab-bar-close-tab)
                      (message "cancelled."))))
         ("s-r" . tab-bar-rename-tab))
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-show t)
  (tab-bar-separator "​​")
  (tab-bar-tab-hints t)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(meta))
  (tab-bar-tab-name-truncated-max 15)
  (tab-bar-border nil)
  (tab-bar-auto-width nil)
  (tab-bar-format '(tab-bar-format-tabs
                    tab-bar-format-add-tab
                    tab-bar-format-align-right
                    +tab-bar-telega-icon))
  ;; tab-bar-tab-name-function #'tab-bar-tab-name-truncated
  ;; tab-bar-tab-name-format-function #'eli/tab-bar-tab-name-with-svg
  (tab-bar-tab-name-function #'+tab-bar-tab-name-function)
  (tab-bar-tab-name-format-function #'+tab-bar-tab-name-format-function)
  (tab-bar-auto-width-max '((200)  20))
  ;; Sessions
  (tabspaces-session t)
  ;; (tabspaces-session-auto-restore t)
  )

;; telega notification
(defvar +tab-bar-telega-indicator-cache nil)

(defun +tab-bar-telega-icon-update (&rest rest)
  "Update the Telega icon in the tab bar, reflecting notification counts.
This function takes REST as an optional argument, though it is not used
within the function body.

The function checks if the Telega server is live and if the server buffer
is active.  It computes various counts, including:

- The number of unread messages (`unread-count`).
- The number of mentions (`mentioned-count`).
- The number of unread reactions (`reaction-count`).
- The number of keyword matches (`keyword-count`).

The total `notification-count` is the sum of these counts.  If this total
is greater than zero, a formatted string with icons and counts is returned.
This string includes:

- A Telegram icon.
- A bullet with the unread count.
- An at-sign with the mention count.
- A heart with the reaction count.
- A hash with the keyword count.

The function uses `nerd-icons-faicon` for the Telegram icon and applies
specific faces to the counts for visual differentiation."
  (setq +tab-bar-telega-indicator-cache
        (when (and (fboundp 'telega-server-live-p)
                   (telega-server-live-p)
                   (buffer-live-p telega-server--buffer))
          (let* ((me-user (telega-user-me 'locally))
                 (online-p (and me-user (telega-user-online-p me-user)))
                 (keyword-count (length (ring-elements telega--notification-messages-ring)))
                 (unread-count (or (plist-get telega--unread-chat-count :unread_unmuted_count) 0))
                 (mentioned-count (apply '+ (mapcar (telega--tl-prop :unread_mention_count)
                                                    (telega-filter-chats telega--ordered-chats '(mention)))))
                 ;; 最好使用 (and is-known unread-reactions) temex 来切断一般列表中不可见的聊天
                 ;; 此类聊天，例如对频道中的帖子发表评论，或者您进入、写下一些内容然后离开，然后有人做出反应的聊天
                 (reaction-count (apply '+ (mapcar (telega--tl-prop :unread_reaction_count)
                                                   (telega-filter-chats telega--ordered-chats '(and is-known unread-reactions)))))
                 (notification-count (+ mentioned-count unread-count reaction-count keyword-count)))
            (when (> notification-count 0)
              (concat "[" (nerd-icons-faicon "nf-fae-telegram" :face '(:inherit nerd-icons-purple))
                      (when (> unread-count 0)
                        (propertize (concat " ●​​​" (number-to-string unread-count))
                                    'face 'telega-unmuted-count))
                      (when (> mentioned-count 0)
                        (propertize (concat " @​​​" (number-to-string mentioned-count))
                                    'face 'telega-mention-count))
                      (when (> reaction-count 0)
                        (propertize (concat " ♥​​​" (number-to-string reaction-count))
                                    'face 'telega-mention-count))
                      (when (> keyword-count 0)
                        (propertize (concat " #​​​" (number-to-string keyword-count))
                                    'face 'telega-unmuted-count))
                      "] "))))))

(defun +tab-bar-telega-icon ()
  "Return the Telega icon for the tab bar, updating if necessary.
This function checks if `+tab-bar-telega-indicator-cache` is set.  If it is,
the cached value is returned.  Otherwise, it calls `+tab-bar-telega-icon-update`
to refresh the icon and returns the updated value."
  (or +tab-bar-telega-indicator-cache
      (+tab-bar-telega-icon-update)))

;; (use-package nano-minibuffer
;; :defer nil
;; :vc (:url https://github.com/rougier/nano-minibuffer))

;; Child frame
(use-package posframe
  :hook (after-load-theme . posframe-delete-all)
  :init
  (defface posframe-border
    `((t (:inherit region)))
    "Face used by the `posframe' border."
    :group 'posframe)
  (defvar posframe-border-width 1
    "Default posframe border width.")
  :config
  (with-no-warnings
    (defun my-posframe--prettify-frame (&rest _)
      (set-face-background 'fringe nil posframe--frame))
    (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

    (defun posframe-poshandler-frame-center-near-bottom (info)
      (cons (/ (- (plist-get info :parent-frame-width)
                  (plist-get info :posframe-width))
               2)
            (/ (+ (plist-get info :parent-frame-height)
                  (* 2 (plist-get info :font-height)))
               2)))))

;; (use-package bufferlo
;;  :ensure t
;;  :init
;;  (bufferlo-mode))

;; (with-eval-after-load "persp-mode-autoloads"
;;   (setq wg-morph-on nil) ;; switch off animation
;;   (setq persp-autokill-buffer-on-remove 'kill-weak)
;;   (add-hook 'window-setup-hook #'(lambda () (persp-mode 1))))

(use-package workgroups2
  :defer t
  :hook
  (after-init . workgroups-mode)
  :custom
  (wg-prefix-key (kbd "C-c w"))
  (wg-session-file "~/.emacs.d/var/workgroups"))


;; ;; Restore Opened Files
;; (progn
;;   (desktop-save-mode 1)
;;   ;; save when quit
;;   (setq desktop-save t)

;;   ;; no ask if crashed
;;   (setq desktop-load-locked-desktop t)
;;   (setq desktop-restore-frames t)
;;   (setq desktop-auto-save-timeout 300)

;;   ;; save some global vars
;;   (setq desktop-globals-to-save nil)
;;   ;; 2023-09-16 default
;;   ;; '(desktop-missing-file-warning tags-file-name tags-table-list search-ring regexp-search-ring register-alist file-name-history)
;;   (setq desktop-dirname "~/.emacs.d/var/desktop/")
;;   )

;; ;; (progn
;; ;;   (require
;; ' desktop-recover)
;;   ;; optionallly:
;;     (setq desktop-recover-location
;;           (desktop-recover-fixdir "~/.emacs.d/var/desktop/"))
;;     ;; Brings up the interactive buffer restore menu
;;     (desktop-recover-interactive)
;;     ;; Note that after using this menu, your desktop will be saved
;;     ;; automatically (triggered by the auto-save mechanism).
;;     ;; For finer-grained control of the frequency of desktop saves,
;;     ;; you can add the standard keybindings to your set-up:
;;     (desktop-recover-define-global-key-bindings "\C-c%")
;;   )


(provide 'init-workspace)
