;; set-font
(defun ding-font-existsp (font)
  (if (null (x-list-fonts font))
      nil
    t))
;; LXGW WenKai Mono 配合 Iosevka 按照 1:1 缩放，偶数字号就可以做到等高等宽。
(defvar zh-font-list '("LXGW WenKai Mono Regular" "TsangerJinKai 02 W04" "LXGW Bright Medium" "HanaMinB"))
(defvar en-font-list '("iosevka" "JetBrains Mono" "Fira Code" "IBM Plex Mono"))

(defun ding-make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defun ding-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-font-scale)

  (setq chinese-font-scale (or chinese-font-scale 1))

  (setq face-font-rescale-alist
        (cl-loop for x in zh-font-list
              collect (cons x chinese-font-scale)))

  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-scale to nil, it will follow english-font-size"

  (let ((en-font (ding-make-font-string
                  (cl-find-if #'ding-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (cl-find-if #'ding-font-existsp chinese-fonts))))

    ;; Set the default English font
    (message "Set English Font to %s" en-font)
    (set-face-attribute 'default nil :font en-font)

    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the English font setting invalid
    (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset zh-font))))

(ding-set-font en-font-list 10 zh-font-list)
(add-to-list 'face-font-rescale-alist '("Apple Color Emoji" . 0.8))

;; (setq font-use-system-font t)

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
;; (defvar frame-transparency '(95 . 95))

;; (set-frame-parameter (selected-frame) 'alpha frame-transparency)
;; (add-to-list 'default-frame-alist `(alpha . ,frame-transparency))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

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

(global-visual-line-mode t)
(require 'visual-fill-column)
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(setq-default visual-fill-column-center-text t)
(setq-default visual-fill-column-width 190)

;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(require 'ef-themes)

(setq custom-safe-themes t)
(setq-default custom-enabled-themes '(ef-trio-light))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)


;; Toggle between light and dark

(defun light ()
  "Activate a light color theme."
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (setq custom-enabled-themes '(doom-opera-light))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (setq custom-enabled-themes '(ef-winter doom-palenight))
  (reapply-themes))

(use-package nerd-icons
  :ensure t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package dimmer
  :ensure t
  :hook (after-init . dimmer-mode)
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (setq-default dimmer-fraction 0.35)
  (with-eval-after-load 'dimmer
    ;; TODO: file upstream as a PR
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all))))
  (with-eval-after-load 'dimmer
    ;; Don't dim in terminal windows. Even with 256 colours it can
    ;; lead to poor contrast.  Better would be to vary dimmer-fraction
    ;; according to frame type.
    (defun sanityinc/display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p))
)


;;set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it
;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 177) (height . 53)))

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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(provide 'ui)
