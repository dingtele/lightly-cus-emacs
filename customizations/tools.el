(require 'package)

  (custom-set-variables '(package-archives '(
                           ("melpa-stable" . "http://stable.melpa.org/packages/")
                           ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                           ("ts-melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           )))
(add-to-list 'package-archives
             '("gnu-devel" . "https://elpa.gnu.org/devel/") :append)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; install use-package manager
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(custom-set-variables       '(use-package-always-ensure t)
                            ;; PERF
                            ;; '(use-package-always-defer t)
                            '(use-package-verbose nil)
                            '(load-prefer-newer t))

(use-package auto-compile
  :defer nil
  :config (auto-compile-on-load-mode))

;; optimize for build-in :vc to avoid long time deep clone with all package's history
(defun my/vc-git-clone (fn remote directory rev)
  (if (or (not (string-match-p "elpa" directory))
          (null rev))
      (funcall fn remote directory rev)
    (cond
     ((ignore-errors
        ;; First try if rev is a branch/tag name
        ;; https://stackoverflow.com/a/48748567/2163429
        (vc-git--out-ok "clone" "--depth" "1" "--single-branch" "--branch" rev remote directory)))
     ((vc-git--out-ok "clone" "--single-branch" remote directory)
      (let ((default-directory directory))
        (vc-git--out-ok "checkout" rev))))
    directory))

(advice-add 'vc-git-clone :around
            'my/vc-git-clone)

;; install the required packages
;; Set missing package vars
(defvar lem-missing-packages '()
  "List populated at startup containing packages needing installation.")
(defvar lem-missing-vc-packages '()
  "List populated at startup containing vc packages requiring installation.")

;; Check for packages
(defun lem-check-missing-packages ()
  "Check for missing packages."
  (interactive)
  ;; Check packages
  (message "%s" "Checking for missing packages.")
  (dolist (p package-selected-packages)
    (unless (package-installed-p p)
      (add-to-list 'lem-missing-packages p 'append)))
  ;; Check vc installed packages (Emacs 29+)
  (when (version< "29" emacs-version)
    (message "%s" "Checking for missing vc packages.")
    (dolist (p package-vc-selected-packages)
      (unless (package-installed-p (car p))
        (add-to-list 'lem-missing-vc-packages (car p) 'append)))))

;; Install packages
(defun lem-install-missing-packages ()
  "Install missing packages from package & package-vc lists."
  (interactive)
  (lem-check-missing-packages)
  (cond ((or lem-missing-packages
             lem-missing-vc-packages)
         (message "Refreshing package database & installing missing packages...")
         (package-install-selected-packages t)
         (setq lem-missing-packages '())
         (package-vc-install-selected-packages)
         (setq lem-missing-vc-packages '()))
        (t
         (message "No missing packages."))))

(add-to-list 'load-path "~/.emacs.d/vendor")
  (add-to-list 'load-path "~/.emacs.d/customizations")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/")

  (require 'editing) ;; -> F2
  (require 'ui) ;; -> F4
  ;; (require 'shell-integration)
  (require 'navigation) ;; -> F3
  (require 'misc)
  ;; (require 'init-site-lisp)
  ;; (require 'init-core-overriding)
  ;; ;; Langauage-specific
  ;; (require 'elisp-editing)
  (require 'init-minibuffer-completion)
(require 'init-org)

;; 快速打开配置文件
(defun open-init-file-and-eval()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  (eval-buffer))

(defun open-editing-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/editing.el"))

(defun open-navigation-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/navigation.el"))

(defun open-ui-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/ui.el"))

(defun open-misc-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/misc.el"))

(defun open-tools-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/tools.org"))

(defun open-task-org-file()
  (interactive)
  (find-file "~/Dropbox/org/Task.org"))

(defun open-org-file()
  (interactive)
  (find-file "~/.emacs.d/customizations/init-org.el"))

(global-set-key (kbd "<f1>") 'open-init-file-and-eval)
(global-set-key (kbd "<f2>") 'open-editing-file)
(global-set-key (kbd "<f3>") 'open-navigation-file)
(global-set-key (kbd "<f4>") 'open-ui-file)
(global-set-key (kbd "<f10>") 'open-task-org-file)
(global-set-key (kbd "<f6>") 'open-misc-file)
(global-set-key (kbd "<f9>") 'open-tools-file)
  (global-set-key (kbd "<f5>") 'open-org-file)


(use-package clipetty
  :ensure t
  :bind ("M-c" . clipetty-kill-ring-save))

(defun keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'keyboard-quit-dwim)

;; completion UI - the front end
 (use-package corfu
   :custom
   (setq corfu-auto t)
   (setq corfu-quit-no-match 'separator)
   :init
   
   (global-corfu-mode)
   :bind (:map corfu-map ("<tab>" . corfu-complete))
   :config
   (setq tab-always-indent 'complete)
   (setq corfu-preview-current nil)
   (setq corfu-min-width 20)

   (setq corfu-popupinfo-delay '(1.25 . 0.5))
   (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

   ;; Sort by input history (no need to modify `corfu-sort-function').
   (with-eval-after-load 'savehist
     (corfu-history-mode 1)
     (add-to-list 'savehist-additional-variables 'corfu-history))
   
   )  

 ;; completion backend
 (use-package cape
   :ensure t
   :bind (("M-/" . completion-at-point))
   :init
   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
   (add-to-list 'completion-at-point-functions #'cape-file)
   (add-to-list 'completion-at-point-functions #'cape-elisp-block)
   (add-to-list 'completion-at-point-functions #'cape-abbrev)
   (add-to-list 'completion-at-point-functions #'cape-dict)
   (add-to-list 'completion-at-point-functions #'cape-line)

   )

;;  ;; child frram beautify
;;   ( use-package nova
;;    :ensure t
;;    :vc (:url https://github.com/thisisran/nova)
;;    :config
;;      (require 'nova)
;;  (require 'nova-vertico)
;;  (require 'nova-corfu)
;;  (require 'nova-corfu-popupinfo)
;; ;; (require 'nova-eldoc)

;;    (nova-vertico-mode 1)
;;    (nova-corfu-mode 1)
;;    (nova-corfu-popupinfo-mode 1)
;;  ;;  (nova-eldoc 1)	
;;   )

;;design a transient key binding
(use-package hydra
  :defer t)
;;use the macro defhydra to define the hydra and its heads
(defhydra hydra-text-scale (global-map "<f12>")
  "scale text"
  ("j" move-line-up "up")
  ("k" move-line-down "down")
  ("f" nil "finished" :exit t))
;; hercules arrives with any other key binding

(use-package shackle
    :config
    (progn
      (setq shackle-lighter "")
      (setq shackle-select-reused-windows nil) ; default nil
      (setq shackle-default-alignment 'below) ; default below
      (setq shackle-default-size 0.4) ; default 0.5

      (setq shackle-rules
            ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
            '((compilation-mode              :select nil                                               )
              ("*undo-tree*"                                                    :size 0.25 :align right)
              ("*eshell*"                    :select t                          :other t               )
              ("*Shell Command Output*"      :select nil                                               )
              ("\\*Async Shell.*\\*"                      :regexp t :ignore t                          )
              (occur-mode                    :select nil                                   :align t     :size 0.3)
              ("*Help*"                      :select t   :inhibit-window-quit nil :other t   :align right)
              (helpful-mode                  :select t                                      :align right)
              ("*Completions*"                                                  :size 0.3  :align t    )
              ("*Messages*"                  :select nil :inhibit-window-quit nil :align below :size 0.3)
              ("\\*[Wo]*Man.*\\*"  :regexp t :select t   :inhibit-window-quit t :other t               )
              ("\\*poporg.*\\*"    :regexp t :select t                          :other t               )
              ("\\`\\*helm.*?\\*\\'" :regexp t                                  :size 0.3  :align t    )
              ("*Calendar*"                  :select t                          :size 0.3  )
              ("*info*"                      :select t   :inhibit-window-quit t                         :same t)
              (magit-status-mode             :select t   :inhibit-window-quit t                         :same t)
              (magit-log-mode                :select t   :inhibit-window-quit t                         :same t)
	      ;; ("*Capture*" :select t :inhibit-window-quit nil :size 0.3 :align right)
              ;; (org-capture-mode :select t :inhibit-window-quit nil :align right :size 0.4)
              ("*Packages*" :select t :inhibit-window-quit nil :same t)
              (pdf-outline-buffer-mode :select t :align below)
              (eshell-mode :select t :align below :size 0.3)
              ("*eshell*" :select t :align below :size 0.3)
              ))

      (shackle-mode 1)))

(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
       ("M-`"   . popper-cycle)
       ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Async Shell Command\\*"
          help-mode
          helpful-mode
          occur-mode
          pass-view-mode
          eshell-mode
          ;; "^\\*eshell.*\\*$" eshell-mode ;; eshell as a popup
          ;; "^\\*shell.*\\*$"  shell-mode  ;; shell as a popup
          ;; ("\\*corfu\\*" . hide)
          (compilation-mode . hide)
          ibuffer-mode
          debugger-mode
          ;; "CAPTURE-Task.org"
          ;; derived from `fundamental-mode' and fewer than 10 lines will be considered a popup
          (lambda (buf) (with-current-buffer buf
                          (and (derived-mode-p 'fundamental-mode)
                               (< (count-lines (point-min) (point-max))
                                  10))))
          )
        )
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
  ;; group by project.el, projectile, directory or perspective
  (setq popper-group-function nil)

  ;; pop in child frame or not
  (setq popper-display-function #'display-buffer-in-child-frame)

  ;; use `shackle.el' to control popup
  (setq popper-display-control nil)
  )

;; (tab-bar-mode 1)
(setq tab-bar-new-button-show nil)
(setq tab-bar-close-button-show nil)
(setq tab-bar-show 1)
(setq tab-bar-tab-hints t) ;; show number
(setq tab-bar-auto-width nil) ;; 取消自动 padding 大小(29.2 引入)
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right tab-bar-format-global))
(defun my/update-tab-bar-after-theme-change (&rest _args)
  "Update tab bar face attributes after a theme change."
  (set-face-attribute 'tab-bar-tab nil
                      :inherit 'doom-modeline-panel
                      :foreground 'unspecified
                      :background 'unspecified)

  (set-face-attribute 'tab-bar nil
                      :foreground (face-attribute 'default :foreground)))

(advice-add 'load-theme :after #'my/update-tab-bar-after-theme-change)
(my/update-tab-bar-after-theme-change)

;; Taken from https://andreyor.st/posts/2020-05-10-making-emacs-tabs-look-like-in-atom/
;; https://github.com/andreyorst/dotfiles/blob/740d346088ce5a51804724659a895d13ed574f81/.config/emacs/README.org#tabline

(defun my/set-tab-theme ()
  (let ((bg (face-attribute 'mode-line :background))
        (fg (face-attribute 'default :foreground))
	(hg (face-attribute 'default :background))
        (base (face-attribute 'mode-line :background))
        (box-width (/ (line-pixel-height) 4)))
    (set-face-attribute 'tab-line nil
			:background base
			:foreground fg
			:height 0.8
			:inherit nil
			:box (list :line-width -1 :color base)
			)
    (set-face-attribute 'tab-line-tab nil
			:foreground fg
			:background bg
			:weight 'normal
			:inherit nil
			:box (list :line-width box-width :color bg))
    (set-face-attribute 'tab-line-tab-inactive nil
			:foreground fg
			:background base
			:weight 'normal
			:inherit nil
			:box (list :line-width box-width :color base))
    (set-face-attribute 'tab-line-highlight nil
			:foreground fg
			:background hg
			:weight 'normal
			:inherit nil
			:box (list :line-width box-width :color hg))
    (set-face-attribute 'tab-line-tab-current nil
			:foreground fg
			:background hg
			:weight 'normal
			:inherit nil
			:box (list :line-width box-width :color hg))))

(defun my/tab-line-name-buffer (buffer &rest _buffers)
  "Create name for tab with padding and truncation.
If buffer name is shorter than `tab-line-tab-max-width' it gets
centered with spaces, otherwise it is truncated, to preserve
equal width for all tabs.  This function also tries to fit as
many tabs in window as possible, so if there are no room for tabs
with maximum width, it calculates new width for each tab and
truncates text if needed.  Minimal width can be set with
`tab-line-tab-min-width' variable."
  (with-current-buffer buffer
    (let* ((window-width (window-width (get-buffer-window)))
           (tab-amount (length (tab-line-tabs-window-buffers)))
           (window-max-tab-width (if (>= (* (+ tab-line-tab-max-width 3) tab-amount) window-width)
                                     (/ window-width tab-amount)
                                   tab-line-tab-max-width))
           (tab-width (- (cond ((> window-max-tab-width tab-line-tab-max-width)
                                tab-line-tab-max-width)
                               ((< window-max-tab-width tab-line-tab-min-width)
                                tab-line-tab-min-width)
                               (t window-max-tab-width))
                         3)) ;; compensation for ' x ' button
           (buffer-name (string-trim (buffer-name)))
           (name-width (length buffer-name)))
      (if (>= name-width tab-width)
          (concat  " " (truncate-string-to-width buffer-name (- tab-width 2)) "…")
        (let* ((padding (make-string (+ (/ (- tab-width name-width) 2) 1) ?\s))
               (buffer-name (concat padding buffer-name)))
          (concat buffer-name (make-string (- tab-width (length buffer-name)) ?\s)))))))

(defun tab-line-close-tab (&optional e)
  "Close the selected tab.
If tab is presented in another window, close the tab by using
`bury-buffer` function.  If tab is unique to all existing
windows, kill the buffer with `kill-buffer` function.  Lastly, if
no tabs left in the window, it is deleted with `delete-window`
function."
  (interactive "e")
  (let* ((posnp (event-start e))
         (window (posn-window posnp))
         (buffer (get-pos-property 1 'tab (car (posn-string posnp)))))
    (with-selected-window window
      (let ((tab-list (tab-line-tabs-window-buffers))
            (buffer-list (flatten-list
                          (seq-reduce (lambda (list window)
                                        (select-window window t)
                                        (cons (tab-line-tabs-window-buffers) list))
                                      (window-list) nil))))
        (select-window window)
        (if (> (seq-count (lambda (b) (eq b buffer)) buffer-list) 1)
            (progn
              (if (eq buffer (current-buffer))
                  (bury-buffer)
                (set-window-prev-buffers window (assq-delete-all buffer (window-prev-buffers)))
                (set-window-next-buffers window (delq buffer (window-next-buffers))))
              (unless (cdr tab-list)
                (ignore-errors (delete-window window))))
          (and (kill-buffer buffer)
               (unless (cdr tab-list)
                 (ignore-errors (delete-window window)))))))))

(unless (version< emacs-version "27")
  (use-package tab-line
    :ensure nil
    ;; :hook (after-init . global-tab-line-mode)
    :config

    (defcustom tab-line-tab-min-width 10
      "Minimum width of a tab in characters."
      :type 'integer
      :group 'tab-line)

    (defcustom tab-line-tab-max-width 30
      "Maximum width of a tab in characters."
      :type 'integer
      :group 'tab-line)

    (setq tab-line-close-button-show t
          tab-line-new-button-show nil
          tab-line-separator ""
          tab-line-tab-name-function #'my/tab-line-name-buffer
          tab-line-right-button (propertize (if (char-displayable-p ?▶) " ▶ " " > ")
                                            'keymap tab-line-right-map
                                            'mouse-face 'tab-line-highlight
                                            'help-echo "Click to scroll right")
          tab-line-left-button (propertize (if (char-displayable-p ?◀) " ◀ " " < ")
                                           'keymap tab-line-left-map
                                           'mouse-face 'tab-line-highlight
                                           'help-echo "Click to scroll left")
          tab-line-close-button (propertize (if (char-displayable-p ?×) " × " " x ")
                                            'keymap tab-line-tab-close-map
                                            'mouse-face 'tab-line-close-highlight
                                            'help-echo "Click to close tab"))

    (my/set-tab-theme)

    ;;(dolist (mode '(ediff-mode process-menu-mode term-mode vterm-mode))
    ;;(add-to-list 'tab-line-exclude-modes mode))
    (dolist (mode '(ediff-mode process-menu-mode))
      (add-to-list 'tab-line-exclude-modes mode))
    ))

;; (global-tab-line-mode t)

(use-package workgroups2
      :init (setq wg-prefix-key (kbd "C-c w"))
      :config
      (workgroups-mode 1)
      (setq wg-session-file "~/.emacs.d/var/workgroups"))

;; Restore Opened Files
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
;; )

;; (progn
;;   (require ' desktop-recover)
;;   ;; optionallly:
;;   (setq desktop-recover-location
;;         (desktop-recover-fixdir "~/.emacs.d/var/desktop/")) 
;;   ;; Brings up the interactive buffer restore menu
;;   (desktop-recover-interactive)
;;   ;; Note that after using this menu, your desktop will be saved
;;   ;; automatically (triggered by the auto-save mechanism).
;;   ;; For finer-grained control of the frequency of desktop saves,
;;   ;; you can add the standard keybindings to your set-up:
;;   (desktop-recover-define-global-key-bindings "\C-c%")
;; )

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/copilot.el-main")
  ;; (require 'copilot)
  ;; (add-hook 'prog-mode-hook 'copilot-mode)
  ;; ;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  ;; (define-key copilot-completion-map (kbd "M-w") 'copilot-accept-completion-by-word)
  ;; (define-key copilot-completion-map (kbd "M-q") 'copilot-accept-completion-by-line)

  (use-package gptel
    :ensure t
    :config
    ;; default backend configuration
    ;; (setq
    ;;  gptel-model "codegeex4:latest"
    ;;  gptel-backend (gptel-make-ollama "Ollama"
    ;;                  :host "localhost:11434"
    ;;                  :stream t
    ;;                  :models '("codegeex4:latest")))

    ;; DeepSeek offers an OpenAI compatible API
    (defun get-openai-api-key ()
      "Return the OpenAI API key from ~/.authinfo."
      (let ((authinfo-file (expand-file-name "~/.authinfo")))
        (with-temp-buffer
          (insert-file-contents authinfo-file)
          (goto-char (point-min))
          (when (re-search-forward "^machine api\\.deepseek\\.com login apikey password \\(\\S-+\\)$" nil t)
            (match-string 1)))))

    (setq gptel-model   "deepseek-chat"
          gptel-backend
          (gptel-make-openai "DeepSeek"     ;Any name you want
            :host "api.deepseek.com"
            :endpoint "/chat/completions"
            :stream t
            :key (get-openai-api-key)             ;can be a function that returns the key
            :models '("deepseek-chat" "deepseek-coder")))

    )

  ;; (use-package immersive-translate
  ;;   :ensure t
  ;;   :config
  ;;   (add-hook 'elfeed-show-mode-hook #'immersive-translate-setup)
  ;;   (add-hook 'nov-pre-html-render-hook #'immersive-translate-setup)
  ;;   )
  ;; (setq immersive-translate-backend 'DeepSeek
  ;;       immersive-translate-chatgpt-host "api.deepseek.com")
(require 'go-translate)
;; (setq gt-langs '(en fr))
(setq gt-preset-translators
      `((ts-1 . ,(gt-translator
                  :taker (gt-taker :langs '(en zh) :text 'buffer)
                  :engines (list (gt-google-engine))
                  :render (gt-overlay-render)))))

(use-package ox-hugo
  :ensure t
  :after ox)

(add-to-list 'load-path "~/.emacs.d/site-lisp/pos-tag-highlight")
(require 'pos-tag-highlight)

(with-eval-after-load 'nov
  (define-key nov-mode-map (kbd "<tab>") 'shrface-outline-cycle)
  (define-key nov-mode-map (kbd "S-<tab>") 'shrface-outline-cycle-buffer)
  (define-key nov-mode-map (kbd "C-t") 'shrface-toggle-bullets)
  (define-key nov-mode-map (kbd "C-j") 'shrface-next-headline)
  (define-key nov-mode-map (kbd "C-k") 'shrface-previous-headline)
  (define-key nov-mode-map (kbd "M-l") 'shrface-links-counsel) ; or 'shrface-links-helm or 'shrface-links-consult
  (define-key nov-mode-map (kbd "M-h") 'shrface-headline-consult)) ; or 'shrface-headline-helm or 'shrface-headline-consult

;;epub reading
  (use-package eww
  :hook (eww-mode . my-nov-font-setup))   

  (use-package nov
       :ensure t
       :mode ("\\.epub\\'" . nov-mode)
       :bind (:map nov-mode-map
                   ("j" . scroll-up-line)
                   ("k" . scroll-down-line)))

     (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
     (setq nov-text-width 80)
     ;; (setq nov-text-width t)
     (setq visual-fill-column-center-text t)
     (add-hook 'nov-mode-hook 'visual-line-mode)
     (add-hook 'nov-mode-hook 'visual-fill-column-mode)

     (add-hook 'nov-mode-hook 'my-nov-font-setup)

     ;;Nov-rendering
     (add-to-list 'load-path "~/.emacs.d/elpa/justify-kp/")
     (require 'justify-kp)
     (use-package justify-kp
       :vc (:url "https://github.com/Fuco1/justify-kp" :rev latest-release))

     (setq nov-text-width t)

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
;;       :defer t				
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
       :commands calibredb
       :bind ("\e\e b" . calibredb)
       :config
       (setq calibredb-root-dir "/Users/dingyu/Documents/calibre")
       (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
       (setq calibredb-library-alist '(("~/Books/books")
                                       ))
       )

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
     ;; (use-package google-translate
     ;;   :defines (google-translate-translation-directions-alist)
     ;;   :bind (("C-c g" . google-translate-smooth-translate))
     ;;   :config
     ;;   (setq google-translate-translation-directions-alist '(("en" . "zh-CN")))
     ;; )

(use-package eglot
  :ensure t
  :hook (python-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '( python-mode . ("pyright")))
  (defun my/eglot-capf ()
    "Set custom completion-at-point functions for Eglot."
    (setq-local completion-at-point-functions '(eglot-completion-at-point)))

  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
  )

;; (use-package eglot
;;   :hook (eglot-managed-mode . my-eglot-mode-hook)
;;   :config
;;   (setq eglot-events-buffer-size 0)
;;   (setq eglot-events-buffer-config '(:size 0 :format full))
;;   (setq eglot-extend-to-xref t)
;;   (setq eglot-autoshutdown t)
;;   (setq eglot-prefer-plaintext t)
;;   (setq eglot-ignored-server-capabilities '(:documentHighlightProvider
;;                                             :documentOnTypeFormattingProvider))
;;   (setq jsonrpc-default-request-timeout 15)
;;   (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename-with-current)
;;   (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-override)
;;   (define-key eglot-mode-map (kbd "C-c i") 'eglot-code-action-organize-imports)
;;   (define-key eglot-mode-map (kbd "C-c h") 'eldoc-box-help-at-point)
;;   (define-key eglot-mode-map (kbd "C-c w r") 'eglot-restart-workspace)
;;   (define-key eglot-mode-map (kbd "C-c v") 'eglot-find-implementation)
;;   (define-key eglot-mode-map (kbd "C-c f") 'eglot-code-actions-current-line)
;;   (define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions))

(require 'eglot)

  (require 'eglot-java)
  (add-hook 'java-mode-hook #'eglot-java-mode)
  (setq eglot-java-server-install-dir "~/codebase/src/java/eclipse.jdt.ls")
  (setq eglot-java-eclipse-jdt-cache-directory "~/tmp/eglot-eclipse-jdt-cache")

;; (require 'ejc-sql)
;; (setq clomacs-httpd-default-port 8090) ; Use a port other than 8080.
;; ;; Require completion frontend (autocomplete or company). One of them or both.
;; (require 'ejc-autocomplete)
;; (add-hook 'ejc-sql-minor-mode-hook
;;           (lambda ()
;;             (auto-complete-mode t)
;;             (ejc-ac-setup)))

;; (setq ejc-use-flx t)
;; (setq ejc-flx-threshold 2)
;; (require 'ejc-company)
;; (push 'ejc-company-backend company-backends)
;; (add-hook 'ejc-sql-minor-mode-hook
;;           (lambda ()
;;             (company-mode t)))
;; (setq ejc-complete-on-dot t)
;; ;; (company-quickhelp-mode t)
;; (setq ejc-completion-system 'standard)

;; (add-hook 'ejc-sql-minor-mode-hook
;;           (lambda ()
;;             (ejc-eldoc-setup)))
;; ;; Performance & output customization
;; (add-hook 'ejc-sql-connected-hook
;;           (lambda ()
;;             (ejc-set-fetch-size 50)
;;             (ejc-set-max-rows 50)
;;             (ejc-set-show-too-many-rows-message t)
;;             (ejc-set-column-width-limit 25)
;;             (ejc-set-use-unicode t)))
;; (setq ejc-result-table-impl 'ejc-result-mode)
;; ;; PostgreSQL example
;; (ejc-create-connection
;;  "PostgreSQL-db-connection"
;;  :classpath (concat "~/.m2/repository/org.postgresql/postgresql/42.6.0/"
;;                     "postgresql-42.6.0.jar")
;;  :subprotocol "postgresql"
;;  :subname "//localhost:5432/postgres"
;;  :user "postgres"
;;  :password "postgres")

(require 'treesit)
(require 'treesit-auto)
(global-treesit-auto-mode t)
(setq treesit-auto-install 'prompt)
(setq treesit-extra-load-path '("~/codebase/src/tree-sitter-module/dist/"))

(use-package s
  :vc (:url "https://github.com/magnars/s.el" :branch master))  
(require 'dash)
(use-package origami :ensure t) 
  (with-eval-after-load 'origami
    (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
    (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes))

(use-package hideshow-org
  :vc (:url "https://github.com/shanecelis/hideshow-org")) 
(global-set-key (kbd"C-c h") 'hs-org/minor-mode)



(use-package rg)

;; eshell
(use-package xterm-color
  :commands (xterm-color-filter))
(use-package eshell
  :after xterm-color
  :config
  (setq eshell-scroll-to-bottom-on-input t)
  (define-key eshell-mode-map (kbd "<tab>") #'company-complete)
  (define-key eshell-hist-mode-map (kbd "M-r") #'consult-history)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-before-prompt-hook (setq xterm-color-preserve-properties t))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))


;; (use-package eshell
;;    :config
;;    (setq eshell-scroll-to-bottom-on-input t)
;;    (setq-local tab-always-indent 'complete)
;;    (setq eshell-history-size 10000)
;;    (setq eshell-save-history-on-exit t) ;; Enable history saving on exit
;;    (setq eshell-hist-ignoredups t) ;; Ignore duplicatesq
;;    :hook
;;    (eshell-mode . my/eshell-hook))

(use-package capf-autosuggest
   :hook
   (eshell-mode . capf-autosuggest-mode))

(defun my/shell-create (name)
   "Create a custom-named eshell buffer with NAME."
   (interactive "sName: ")
   (eshell 'new)
   (let ((new-buffer-name (concat "*eshell-" name "*")))
     (rename-buffer new-buffer-name t)))

(global-set-key (kbd "C-c s") #'my/shell-create)

(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)
(global-set-key "\C-h\C-f" 'find-function-at-point)

;; Enable desired features for all lisp modes
(require 'clojure-ts-mode)
(setq clojure-ts-grammar-recipes nil)

(require 'clojure-mode)
(setq clojure-indent-style 'always-indent
        clojure-indent-keyword-style 'always-indent
        clojure-enable-indent-specs nil)

(require 'cljsbuild-mode)
(require 'elein)

(defun sanityinc/enable-check-parens-on-save ()
      "Run `check-parens' when the current buffer is saved."
      (add-hook 'after-save-hook #'check-parens nil t))

(defvar sanityinc/lispy-modes-hook
      '(enable-paredit-mode
        sanityinc/enable-check-parens-on-save)
      "Hook run in all Lisp modes.")
(add-to-list 'sanityinc/lispy-modes-hook 'aggressive-indent-mode)

(defun sanityinc/lisp-setup ()
      "Enable features useful in any Lisp mode."
      (run-hooks 'sanityinc/lispy-modes-hook))

(with-eval-after-load 'clojure-mode
    (dolist (m '(clojure-mode-hook clojure-ts-mode-hook))
      (add-hook m 'sanityinc/lisp-setup)))

  (require 'cider)
  (setq nrepl-popup-stacktraces nil)
  (add-hook 'clojure-ts-mode-hook #'cider-mode)

    ;; (with-eval-after-load 'cider
    ;;   (add-hook 'cider-repl-mode-hook 'subword-mode)
    ;;   (add-hook 'cider-repl-mode-hook 'paredit-mode))

  ;; (require 'flycheck-clojure)
  ;; (with-eval-after-load 'clojure-ts-mode
  ;;     (with-eval-after-load 'cider
  ;;       (with-eval-after-load 'flycheck
  ;;         (flycheck-clojure-setup))))



(setq tab-always-indent 'complete)
  (setq python-indent-offset 4)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max)))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(auth-source-pass-enable)

(custom-set-variables '(confirm-kill-processes nil))

(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(require 'cl-lib) 

(use-package async)

(add-hook 'before-save-hook 'time-stamp)

(custom-set-variables '(kill-whole-line t))

(custom-set-variables '(mouse-yank-at-point t))

(setq completion-ignore-case t)
(custom-set-variables
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t))

(custom-set-variables '(show-trailing-whitespace nil))

(use-package imenu-anywhere
  :bind
  ("M-i" . imenu-anywhere))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-variables '(ad-redefinition-action (quote accept)))

(bind-key "M-g" 'goto-line)

(bind-key "M-`" 'other-frame)

 ;; 将原本放在 .emacs.d 目录下的一些配置信息或动态信息，转移到 etc 或 var 子目录里，让配置目录更加简洁清爽
 (use-package no-littering
   :ensure t)

 (provide 'tool)
