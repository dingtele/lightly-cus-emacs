;; Customizations relating to editing a buffer.

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-editing-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/editing.el"))

(defun open-navigation-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/navigation.el"))

(defun open-ui-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/ui.el"))

(defun open-init-org-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/init-org.el"))

(defun open-misc-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/misc.el"))

(defun open-tools-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/tools.org"))

(global-set-key (kbd "<f1>") 'open-init-file)
(global-set-key (kbd "<f2>") 'open-editing-file)
(global-set-key (kbd "<f3>") 'open-navigation-file)
(global-set-key (kbd "<f4>") 'open-ui-file)
(global-set-key (kbd "<f5>") 'open-init-org-file)
(global-set-key (kbd "<f6>") 'open-misc-file)
(global-set-key (kbd "<f9>") 'open-tools-file)

;; auto completion of function name
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list  
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
;; Highlights matching parenthesis(highlight paren or expression)
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(save-place-mode 1)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(delete-selection-mode t)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))                     

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode nil)

(defun previous-multilines ()
  "scroll down multiple lines"
  (interactive)
  (scroll-down (/ (window-body-height) 3)))

(defun next-multilines ()
  "scroll up multiple lines"
  (interactive)
  (scroll-up (/ (window-body-height) 3)))

(global-set-key "\M-n" 'next-multilines) ;;custom
(global-set-key "\M-p" 'previous-multilines) ;;custom
;; Move line up
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

;; Move line down
(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

;; Assign the custom keybindings
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(global-set-key (kbd "M-o") 'other-window)
(windmove-default-keybindings)

(use-package clipetty
  :ensure t
  :bind ("M-w" . clipetty-kill-ring-save))

;;key-bindings
(defun select-current-line ()
  "Select the line at the current cursor position."
  (interactive)
  (let ((line-begin (line-beginning-position))
        (line-end (line-end-position)))
    (goto-char line-begin)
    (set-mark line-end)))

(global-set-key (kbd "C-c C-s") 'select-current-line)
;; (defun find-file()
;;   (kbd "C-x C-f"))
;; (global-set-key (kbd "C-f") 'find-file)
(global-set-key (kbd "C-x g") 'magit-status)
;; Shift lines up and down withM-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.

;;design a transient key binding
(use-package hydra
  :defer t)
;;use the macro defhydra to define the hydra and its heads
(defhydra hydra-text-scale (global-map "<f12>")
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))
;; hercules arrives with any other key binding

;; (eval(define-key dired-mode-map "c" 'find-file)

(defun add-space-between-chinese-and-english ()
  "在中英文之间自动添加空格。"
  (let ((current-char (char-before))
        (prev-char (char-before (1- (point)))))
    (when (and current-char prev-char
               (or (and (is-chinese-character prev-char) (is-halfwidth-character current-char))
                   (and (is-halfwidth-character prev-char) (is-chinese-character current-char)))
               (not (eq prev-char ?\s))) ; 检查前一个字符不是空格
      (save-excursion
        (goto-char (1- (point)))
        (insert " ")))))

(defun is-chinese-character (char)
  "判断字符是否为中文字符。"
  (and char (or (and (>= char #x4e00) (<= char #x9fff))
                (and (>= char #x3400) (<= char #x4dbf))
                (and (>= char #x20000) (<= char #x2a6df))
                (and (>= char #x2a700) (<= char #x2b73f))
                (and (>= char #x2b740) (<= char #x2b81f))
                (and (>= char #x2b820) (<= char #x2ceaf)))))

(defun is-halfwidth-character (char)
  "判断字符是否为半角字符，包括英文字母、数字和标点符号。"
  (and char (or (and (>= char ?a) (<= char ?z))
                (and (>= char ?A) (<= char ?Z))
                (and (>= char ?0) (<= char ?9))
                )))

(defun delayed-add-space-between-chinese-and-english ()
  "延迟执行，在中英文之间自动添加空格。"
  (run-with-idle-timer 0 nil 'add-space-between-chinese-and-english))

(define-minor-mode auto-space-mode
  "在中英文之间自动添加空格的模式。"
  :lighter " Auto-Space"
  :global t
  (if auto-space-mode️
      (add-hook 'post-self-insert-hook 'add-space-between-chinese-and-english)
    (remove-hook 'post-self-insert-hook 'add-space-between-chinese-and-english)))

(defun get-sentence-around-word ()
  "Capture the sentence around the current word."
  (interactive)
  (let* ((pos-start (point))
         (pos-end pos-start))
    ;; Move backward until we find the start of the sentence
    (skip-syntax-backward "-")
    
    ;; If at the beginning of a buffer, set the start position to the beginning of the buffer
    (when (eq (char-before) nil)
      (setq pos-start (point-min)))
    
    ;; Move forward until we find the end of the sentence
    (skip-syntax-forward "w")
    
    ;; Mark the beginning of the sentence area
    (set-mark-command nil)
    
    ;; Save the current buffer position and mark as the end position
    (setq pos-end (point))
    
    ;; Go back to the start of the sentence
    (goto-char pos-start)
    
    ;; Select the marked area
    (exchange-point-and-mark)

    ;; Return the text within the sentence area
    (buffer-substring-no-properties (region-beginning) (region-end))))

(global-set-key (kbd "C-c C-s") 'get-sentence-around-word)




(provide 'editing)
