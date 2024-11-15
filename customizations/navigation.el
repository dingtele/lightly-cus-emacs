;; ;; "When several buffers visit identically-named files,
;; ;; Emacs must give the buffers distinct names. The usual method
;; ;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; ;; of the buffer names (all but one of them).
;; ;; The forward naming method includes part of the file's directory
;; ;; name at the beginning of the buffer name
;; ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
;; (require 'uniquify)
;; (setq uniquify-buffer-name-style 'forward)

;; ;; Turn on recent file mode so that you can more easily switch to
;; ;; recently edited files when you first start emacs
;; (setq recentf-save-file (concat user-emacs-directory ".recentf"))
;; (require 'recentf)
;; (recentf-mode 1)
;; (setq recentf-max-menu-items 40)

;; projectile everywhere!
;(projectile-global-mode)

(use-package corfu
  :ensure t
  :init
  (progn
    (setq corfu-auto t)
    (setq corfu-cycle t)
    (setq corfu-quit-at-boundary t)
    (setq corfu-quit-no-match t)
    (setq corfu-preview-current nil)
    (setq corfu-min-width 80)
    (setq corfu-max-width 100)
    (setq corfu-auto-delay 0.2)
    (setq corfu-auto-prefix 1)
    (setq corfu-on-exact-match nil)
    (global-corfu-mode)
    ))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  ;; Whether display the icons.
(setq nerd-icons-ibuffer-icon t)
(setq nerd-icons-ibuffer-color-icon t)
(setq nerd-icons-ibuffer-icon-size 1.0)
(setq  nerd-icons-ibuffer-human-readable-size t)
;; A list of ways to display buffer lines with `nerd-icons'.
;; See `ibuffer-formats' for details.
nerd-icons-ibuffer-formats

;; Slow Rendering
;; If you experience a slow down in performance when rendering multiple icons simultaneously,
;; you can try setting the following variable
(setq inhibit-compacting-font-caches t))

(use-package ibuffer-project
  :hook (ibuffer . (lambda ()
                     "Group ibuffer's list by project."
                     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
                     (unless (eq ibuffer-sorting-mode 'project-file-relative)
                       (ibuffer-do-sort-by-project-file-relative))))
  :init (setq ibuffer-project-use-cache t)
  :config
  (defun my-ibuffer-project-group-name (root type)
    "Return group name for project ROOT and TYPE."
    (if (and (stringp type) (> (length type) 0))
        (format "%s %s" type root)
      (format "%s" root)))
  (progn
        (advice-add #'ibuffer-project-group-name :override #'my-ibuffer-project-group-name)
        (setq ibuffer-project-root-functions
              `((ibuffer-project-project-root . ,(nerd-icons-octicon "nf-oct-repo" :height 1.2 :face ibuffer-filter-group-name-face))
                (file-remote-p . ,(nerd-icons-codicon "nf-cod-radio_tower" :height 1.2 :face ibuffer-filter-group-name-face)))))
    (progn
      (advice-remove #'ibuffer-project-group-name #'my-ibuffer-project-group-name)
      (setq ibuffer-project-root-functions
            '((ibuffer-project-project-root . "Project")
              (file-remote-p . "Remote"))))
    (setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              project-relative-file))))


;; (use-package buffer-name-relative
;;   :ensure t
;;   :vc (:url "https://codeberg.org/ideasman42/emacs-buffer-name-relative")
;;   :hook (after-init . buffer-name-relative-mode)
;;   :config
;;   (setq buffer-name-relative-prefix '("" . "/")))
 
;; (use-package ibuffer-sidebar
;;   :load-path "~/.emacs.d/fork/ibuffer-sidebar"
;;   :ensure nil
;;   :commands (ibuffer-sidebar-toggle-sidebar)
;;   :config
;;   (setq ibuffer-sidebar-use-custom-font t)
;;   (setq ibuffer-sidebar-face `(:family "Helvetica" :height 140)))

;; (defun +sidebar-toggle ()
;;   "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
;;   (interactive)
;;   (dired-sidebar-toggle-sidebar)
;;   (ibuffer-sidebar-toggle-sidebar))

(provide 'navigation)
