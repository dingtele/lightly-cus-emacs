(when *IS-MAC*
  ;; modify meta from ⌥ to ⌘
  (custom-set-variables
   '(mac-command-modifier 'meta)
   '(mac-option-modifier 'super)
   ;; '(mac-option-modifier 'alt)
   ;; '(mac-right-option-modifier 'super)
   )
  (bind-keys ([(super l)] . goto-line))
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (use-package exec-path-from-shell
    :defer t
    :config
    (exec-path-from-shell-initialize))
  )

;; Frame
(when (display-graphic-p)
  ;; Frame fullscreen
  (add-hook 'window-setup-hook #'fix-fullscreen-cocoa)
  ;; Frame transparence
  (use-package transwin
    :bind (("C-M-9" . transwin-inc)
           ("C-M-8" . transwin-dec)
           ("C-M-7" . transwin-toggle))
    :init
    (when *IS-LINUX*
      (setq transwin-parameter-alpha 'alpha-background))))

;; PERF
;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

(use-package auto-save
  :defer nil
  :vc (:url "https://github.com/manateelazycat/auto-save")
  :custom
  (auto-save-silent t)   ; quietly save
  (auto-save-idle 3)
  ;; (setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving
  :config
  (auto-save-enable)
  ;;; custom predicates if you don't want auto save.
  ;;; disable auto save mode when current filetype is an gpg file.
  (setq auto-save-disable-predicates
        '((lambda ()
            (string-suffix-p
             "gpg"
             (file-name-extension (buffer-name)) t)))))

;; close the frame by confirmation
(bind-key "C-x C-c" (lambda() (interactive)
                      (if (y-or-n-p "really want to kill the terminal?")
                          ;; (progn (tabspaces-save-session))
                          (save-buffers-kill-terminal)
                        (message "cancelled."))))

(provide 'init-base)
