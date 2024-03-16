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

(global-set-key (kbd "<f1>") 'open-init-file)
(global-set-key (kbd "<f2>") 'open-editing-file)
(global-set-key (kbd "<f3>") 'open-navigation-file)
(global-set-key (kbd "<f4>") 'open-ui-file)
(global-set-key (kbd "<f5>") 'open-init-org-file)

(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

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


;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

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

(use-package clipetty
  :ensure t
  :bind ("M-w" . clipetty-kill-ring-save))


(global-visual-line-mode t)
(use-package visual-fill-column :ensure t)
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(setq-default visual-fill-column-center-text) t
(setq org-image-actual-width nil)
(setq visual-fill-column-width 150)

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
(global-set-key (kbd "C-f") 'find-file)
(global-set-key (kbd "C-x g") 'magit-status)
;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.

;;config ivy mode
;;(ivy-mode)
;; (setq ivy-use-virtual-buffers t)
;; (setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
