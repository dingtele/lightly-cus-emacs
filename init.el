;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; global keyword to turn on / off:
;; TODO: review the operation in detail when it is needed
;; PERF: operations that is possible to cause performance issue, modify accordingly


;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

;; (toggle-enable-multibyte-characters t)
;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; ignore native compile warning
(setq warning-minimum-level :emergency)
;; (setq default-directory "/home/jordi/")

;; PERF
(when (require 'gcmh nil t)
  (gcmh-mode 1))

(setq gc-cons-threshold most-positive-fixnum)

(defconst *IS-MAC* (eq system-type 'darwin))
(defconst *IS-LINUX* (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst *IS-WINDOWS*  (memq system-type '(cygwin windows-nt ms-dos)))

(when *IS-MAC*
  ;; modify meta from ⌥ to ⌘
  (custom-set-variables 
   '(mac-command-modifier 'meta)
   '(mac-option-modifier 'super)
   ;; '(mac-option-modifier 'alt)
   ;; '(mac-right-option-modifier 'super)
   )
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (use-package exec-path-from-shell
    :defer nil
    :config
    (exec-path-from-shell-initialize)))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Maybe some code up here ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org)
(org-babel-tangle-file "~/.emacs.d/customizations/tools.org"
                       "~/.emacs.d/customizations/tools.el")

(load "~/.emacs.d/customizations/tools.el")

(setq gc-cons-threshold (* 2 1000 1000))


;; Customize


(provide 'init)
