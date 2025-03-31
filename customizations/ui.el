(menu-bar-mode 1)

;; Set up the visible bell
(setq visible-bell t)
;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)
;(toggle-frame-maximized)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;;modeline上显示我的所有的按键和执行的命令
(require 'keycast)
(keycast-header-line-mode t)

;(setq-default cursor-type '(bar . 5))
(column-number-mode)
(global-display-line-numbers-mode t)

;; Set frame transparency
;; Make frame transparency overridable
(defvar frame-transparency '(95 . 95))

;; (set-frame-parameter (selected-frame) 'alpha frame-transparency)
;; (add-to-list 'default-frame-alist `(alpha . ,frame-transparency))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (if (display-graphic-p)
                (menu-bar-mode 1)
              (menu-bar-mode -1))))


;; hide the frame title
;; (add-to-list 'default-frame-alist '(undecorated . t))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
;; (set-fringe-mode 2)        ; Give some breathing room

(global-visual-line-mode t)
(require 'visual-fill-column)
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(setq-default visual-fill-column-center-text t)
(setq-default visual-fill-column-width 120)



(use-package nerd-icons
  :ensure t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )


;; ;; These settings relate to how emacs interacts with your operating system
;; (setq ;; makes killing/yanking interact with the clipboard
;;       x-select-enable-clipboard t

;;       ;; I'm actually not sure what this does but it's recommended?
;;       x-select-enable-primary t

;;       ;; Save
;; (add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(add-hook 'switch-buffer-functions
          (lambda (prev curr)
            (cl-assert (eq curr (current-buffer)))  ;; Always t
            (message "%S -> %S" prev curr))) ;;TODO

;; emacs windows configuration layout stack
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :commands (winner-undo winner-redo)
  :config
  (setq winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*esh command on file*"))
  )

(provide 'ui)
