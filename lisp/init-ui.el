(menu-bar-mode 1)

;; Set up the visible bell
(setq visible-bell t)
;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)
                                        ;(toggle-frame-maximized)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;;modeline 上显示我的所有的按键和执行的命令
(require 'keycast)
(keycast-header-line-mode t)
(column-number-mode)

;; Show line numbers
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode messages-buffer-mode backtrace-mode conf-mode toml-ts-mode
                    yaml-mode yaml-ts-mode)
         . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t))

;; Set frame transparency
;; Make frame transparency overridable
(defvar frame-transparency '(95 . 95))

;; (set-frame-parameter (selected-frame) 'alpha frame-transparency)
;; (add-to-list 'default-frame-alist `(alpha . ,frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (if (display-graphic-p)
                (menu-bar-mode 1)
              (menu-bar-mode -1))))


;; hide the frame title
(add-to-list 'default-frame-alist '(undecorated . t))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(scroll-bar-mode 1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
;; (set-fringe-mode 2)        ; Give some breathing room

(global-visual-line-mode t)
(require 'visual-fill-column)
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(setq-default visual-fill-column-center-text t)
(setq-default visual-fill-column-width 150)


;; Easily adjust the font size in all frames
(use-package default-text-scale
  :hook (after-init . default-text-scale-mode)
  :bind (:map default-text-scale-mode-map
         ("s-="   . default-text-scale-increase)
         ("s--"   . default-text-scale-decrease)
         ("s-0"   . default-text-scale-reset)
         ("C-s-=" . default-text-scale-increase)
         ("C-s--" . default-text-scale-decrease)
         ("C-s-0" . default-text-scale-reset)))


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

;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.

;; Don't prompt to confirm theme safety.
(setq custom-safe-themes t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(require 'ef-themes)
;; (require 'nano-theme)

;; (custom-set-variables '(ef-autumn))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

;; my favorite themes for frequent switching:
;; light: doom-feather-light /
;; dark: doom-one /doom-palenight

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
  (setq custom-enabled-themes '(doom-one ef-winter doom-palenight))
  (reapply-themes))

;;; 正色
(defconst n-青       "􀝦#00ffff")
(defconst n-赤       "􀝦#c3272b")
(defconst n-白       "􀝦#ffffff")  ;; 精白
(defconst n-黑       "􀝦#000000")
(defconst n-黄       "􀝦#fff143")  ;; 不知其法而用鵝黃


;;; 間色
(defconst n-紺青     "􀝦#3f4470")
(defconst n-鴉青     "􀝦#424c50")
(defconst n-靛藍     "􀝦#065279")
(defconst n-羣青     "􀝦#2e59a7")
(defconst n-深竹月   "􀝦#2e62cd")
(defconst n-寶藍     "􀝦#4b5cc4")
(defconst n-青冥     "􀝦#3271ae")
(defconst n-靛青     "􀝦#177CB0")
(defconst n-湖藍     "􀝦#30DFF3")
(defconst n--青      "􀝦#00ffff")

(defconst n-松绿     "􀝦#057748")
(defconst n-官緑     "􀝦#2a6e3f")
(defconst n-青青     "􀝦#4f6f46")
(defconst n-蒼翠     "􀝦#519a73")
(defconst n-菉竹     "􀝦#698e6a")
(defconst n-竹靑     "􀝦#789262")
(defconst n-春辰     "􀝦#a9be7b")
(defconst n-松花     "􀝦#bce672")
(defconst n-歐碧     "􀝦#c0d695")
(defconst n-龍泉靑瓷 "􀝦#c8e6c6")
(defconst n-水緑     "􀝦#d4f2e7")
(defconst n-水黄     "􀝦#ddeec4")
(defconst n-春緑     "􀝦#e3efd1")
(defconst n-蔥青     "􀝦#edfebb")
(defconst n-断肠     "􀝦#ecebc2")

(defconst n-絳       "􀝦#510312")
(defconst n-胭脂     "􀝦#960018")
(defconst n-綪       "􀝦#b13546")
(defconst n-品红     "􀝦#F00056")
(defconst n-朱       "􀝦#ff0000")
(defconst n-火红     "􀝦#FF2D51")
(defconst n-丹       "􀝦#ff4c00")
(defconst n-妃       "􀝦#ed5736")
(defconst n-海棠     "􀝦#DB5A6B")
(defconst n-桃红     "􀝦#f47983")
(defconst n-鳳仙粉   "􀝦#FF9393")
(defconst n-粉红     "􀝦#ffb3a7")
(defconst n-露玫瑰   "􀝦#ffe4e1")


(defconst n-墨       "􀝦#50616D")
(defconst n-蒼青     "􀝦#7397ab")
(defconst n-墨灰     "􀝦#758A99")

(defconst n-养生主   "􀝦#b49b7f")

(defconst n-茶       "􀝦#B35C44")
(defconst n-鱼肚     "􀝦#FCEFE8")
(defconst n-珈琲椶   "􀝦#705438")
(defconst n-紙棕     "􀝦#D2B38C")
(defconst n-向日黃   "􀝦#FFC34D")
(defconst n-缟       "􀝦#F2ECDE")
(defconst n-牙       "􀝦#EEDEB0")
(defconst n-米灰     "􀝦#D3CBAF")
(defconst n-芽灰     "􀝦#E3DBBF")
(defconst n-胡粉     "􀝦#FFFAE8")
(defconst n-蠟白     "􀝦#FEF8DE")
(defconst n-富春紡   "􀝦#FEF4B4")
(defconst n-鹅黄     "􀝦#FFF143")
(defconst n-嬭油     "􀝦#fffdd0")
(defconst n-鸭黄     "􀝦#FAFF72")
(defconst n-蛤粉     "􀝦#fdfff4")
(defconst n-荼       "􀝦#F3F9F1")
(defconst n-素       "􀝦#E0F0E9")
(defconst n-霜       "􀝦#E9F1F6")
(defconst n-漆       "􀝦#161823")
(defconst n-黛       "􀝦#4A4266")
(defconst n-丁香     "􀝦#CCA4E3")
(defconst n-青莲     "􀝦#801DAE")
(defconst n-淡紫丁香 "􀝦#e6cfe6")
(defconst n-水紅     "􀝦#f3d3e7")
(defconst n-長萅蕐   "􀝦#FF47D1")
(defconst n-紫扇貝   "􀝦#923A60")

(defun ding-font-existsp (font)
  (if (null (x-list-fonts font))
      nil
    t))
;; LXGW WenKai Mono 配合 Iosevka 按照 1:1 缩放，偶数字号就可以做到等高等宽。
(defvar zh-font-list '("AI Kai" "TsangerJinKai04 W04" "LXGW Bright GB" "LXGW Bright Medium" "HanaMinB"))
(defvar en-font-list '("PragmataPro Mono" "LXGW Bright code GB" "Iosevka Fixed SS14" "JetBrains Maple Mono" "JetBrains Mono" "Fira Code" "IBM Plex Mono"))
(defvar font-size
  (cond (*IS-LINUX* 14)
        (*IS-MAC* 18)))

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

  ;; "english-font-size could be set to \":pixelsize=18\" or a integer.
  ;;   If set/leave chinese-font-scale to nil, it will follow english-font-size"
  (let ((en-font (ding-make-font-string
                  (cl-find-if #'ding-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (cl-find-if #'ding-font-existsp chinese-fonts) :size chinese-font-scale)))
    
    ;; Set the default English font
    (set-face-attribute 'default nil :font en-font)

    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the English font setting invalid
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset zh-font))))

    ;;;;;;  set fonts HERE!!! ;;;;;;
(ding-set-font en-font-list 12 zh-font-list 16)
(add-to-list 'face-font-rescale-alist '("Apple Color Emoji" . 0.8))

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
(setq initial-frame-alist '((top . 50) (left . 100) (width . 177) (height . 53)))

;; https://t.me/emacs_china/263544
(use-package pulse
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region :background unspecified))))
  (pulse-highlight-face ((t (:inherit region :background unspecified :extend t))))
  :hook (((dumb-jump-after-jump imenu-after-jump) . +recenter-and-pulse)
         ((bookmark-after-jump magit-diff-visit-file next-error) . +recenter-and-pulse-line)
         (focus-in . pulse-momentary-highlight-one-line))
  :init
  (setq pulse-delay 0.1
        pulse-iterations 2)

  (defun +pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun +pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (+pulse-momentary-line)))

  (defun +recenter-and-pulse(&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (+pulse-momentary))

  (defun +recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (+pulse-momentary-line))

  (dolist (cmd '(recenter-top-bottom
                 other-window switch-to-buffer
                 aw-select toggle-window-split
                 windmove-do-window-select
                 pager-page-down pager-page-up
                 treemacs-select-window
                 tab-bar-select-tab))
    (advice-add cmd :after #'+pulse-momentary-line))

  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 goto-last-change))
    (advice-add cmd :after #'+recenter-and-pulse))

  (dolist (cmd '(symbol-overlay-basic-jump
                 compile-goto-error))
    (advice-add cmd :after #'+recenter-and-pulse-line))
  )

(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode org-mode emacs-lisp-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse nil)
  )

;; (require 'nano-modeline)
;; (add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
;; (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
;; (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
;; (add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
;; (add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
;; (add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
;; (add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
;; (add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
;; (add-hook 'term-mode-hook            #'nano-modeline-term-mode)
;; (add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
;; (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
;; (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
;; (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)

;; (custom-set-variables '(mode-line-format nil))
;; (nano-minibuffer-mode 1)
;; (nano-modeline-text-mode t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(provide 'init-ui)
