(menu-bar-mode -1)
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

(set-frame-parameter (selected-frame) 'alpha frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
(set-fringe-mode 10)        ; Give some breathing room

;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
;(require 'ef-themes)
;(require 'gruvbox-theme
(load-theme 'ef-cyprus t)
;; (use-package doom-themes)
;; (load-theme 'doom-one 1)
;; (load-theme 'doom-palenight t)

;; set-font
;; (setq font-use-system-font t)
;; (set-fontset-font t nil "Symbola" nil 'prepend)
(defvar font-size (cond ((eq t *IS-MAC*) 16.5)
                        (eq t *IS-WINDOWS*) 12.5))
(set-face-attribute
   'default nil
   :font (font-spec :name "JetBrains Mono"
                    :Weight 'normal
                    :slant 'normal
		    :size font-size))

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset
   (font-spec :name "TsangerJinKai02"
   ;(font-spec :name "LXGW WenKai"
              :weight 'normal
              :slant 'normal
              :size
              17.5)))

;; ;; Uncomment the lines below by removing semicolons and play with the
;; ;; values in order to set the width (in characters wide) and height
;; ;; (in lines high) Emacs will have whenever you start it
;; ;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 177) (height . 53)))

;; ;; These settings relate to how emacs interacts with your operating system
;; (setq ;; makes killing/yanking interact with the clipboard
;;       x-select-enable-clipboard t

;;       ;; I'm actually not sure what this does but it's recommended?
;;       x-select-enable-primary t

;;       ;; Save clipboard strings into kill ring before replacing them.
;;       ;; When one selects something in another program to paste it into Emacs,
;;       ;; but kills something in Emacs before actually pasting it,
;;       ;; this selection is gone unless this variable is non-nil
;;       save-interprogram-paste-before-kill t

;;       ;; Shows all options when running apropos. For more info,
;;       ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
;;       apropos-do-all t

;;       ;; Mouse yank commands yank at point instead of at click.
;;       mouse-yank-at-point t)

;; ;; No cursor blinking, it's distracting
;; (blink-cursor-mode 0)

;; ;; full path in title bar
;; (setq-default frame-title-format "%b (%f)")

;; ;; don't pop up font menu
;; ;; (global-set-key (kbd "s-t") '(lambda () (interactive)))

;; ;; no bell
;; (setq ring-bell-function 'ignore)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(provide 'ui)
