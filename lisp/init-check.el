;; init-check.el --- Initialize check configurations.	-*- lexical-binding: t -*-

;;; Code:

(use-package flymake
  :diminish
  :functions my-elisp-flymake-byte-compile
  :bind ("C-c f" . flymake-show-buffer-diagnostics)
  :hook (prog-mode . flymake-mode)
  :init (setq flymake-no-changes-timeout nil
              flymake-fringe-indicator-position 'right-fringe)
  :config
  ;; Check elisp with `load-path'
  (defun my-elisp-flymake-byte-compile (fn &rest args)
    "Wrapper for `elisp-flymake-byte-compile'."
    (let ((elisp-flymake-byte-compile-load-path
           (append elisp-flymake-byte-compile-load-path load-path)))
      (apply fn args)))
  (advice-add 'elisp-flymake-byte-compile :around #'my-elisp-flymake-byte-compile))

;; (use-package flymake-popon
;;   :diminish
;;   :load-path "~/.emacs.d/elpa/flymake-popon-0.5.1"
;;   :custom-face
;;   (flymake-popon ((t :inherit default :height 0.85)))
;;   (flymake-popon-posframe-border ((t :foreground ,(face-background 'posframe-border nil t))))
;;   :hook (flymake-mode . flymake-popon-mode)
;;   :init (setq flymake-popon-width 70
;;               flymake-popon-posframe-border-width 1
;;               flymake-popon-method 'posframe)) ;'popon

(use-package flyover
  :vc (:url "https://github.com/konrad1977/flyover" :rev :newest)
  :hook (flycheck-mode . flyover-mode)
  :config
  ;; Configure which error levels to display
  ;; Possible values: error, warning, info
  (setq flyover-levels '(error warning info))  ; Show all levels
  ;; (setq flyover-levels '(error warning))    ; Show only errors and warnings
  ;; (setq flyover-levels '(error))            ; Show only errors
  
  ;; Use theme colors for error/warning/info faces
  (setq flyover-use-theme-colors t)
  ;; Adjust background lightness (lower values = darker)
  (setq flyover-background-lightness 45)
  ;; Make icon background darker than foreground
  (setq flyover-percent-darker 40)
  (setq flyover-text-tint 'lighter) ;; or 'darker or nil
  ;; "Percentage to lighten or darken the text when tinting is enabled."
  (setq flyover-text-tint-percent 50)

  ;; Choose which checkers to use (flycheck, flymake, or both)
  (setq flyover-checkers '(flycheck flymake))
  ;; Enable debug messages
  (setq flyover-debug nil)

  ;; Time in seconds to wait before checking and displaying errors after a change
  (setq flyover-debounce-interval 0.2)

  ;; Number of lines below the error line to display the overlay
  ;; Default is 1 (next line), set to 0 for same line, 2 for two lines below, etc.
  (setq flyover-line-position-offset 1)
  ;; Enable wrapping of long error messages across multiple lines
  (setq flyover-wrap-messages t)

  ;; Maximum length of each line when wrapping messages
  (setq flyover-max-line-length 80)
  ;;; Icons
  (setq flyover-info-icon "🛈")
  (setq flyover-warning-icon "⚠")
  (setq flyover-error-icon "✘")

  ;;; Icon padding

  ;;; You might want to adjust this setting if you icons are not centererd or if you more or less space.fs
  (setq flyover-icon-left-padding 0.9)
  (setq flyover-icon-right-padding 0.9)
  (setq flyover-virtual-line-type 'curved-dotted-arrow)

  ;;; Overide virtual-line-type with your own
  (setq flyover-virtual-line-icon "╰──") ;;; default its nil
  ;;; Hide checker name for a cleaner UI
  (setq flyover-hide-checker-name t)

;;; show at end of the line instead.
  (setq flyover-show-at-eol t)

;;; Hide overlay when cursor is at same line, good for show-at-eol.
  (setq flyover-hide-when-cursor-is-on-same-line t) 

;;; Show an arrow (or icon of your choice) before the error to highlight the error a bit more.
  (setq flyover-show-virtual-line t)
  )

(provide 'init-check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-check.el ends here
