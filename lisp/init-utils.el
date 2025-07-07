;; Persistent the scratch buffer
(use-package persistent-scratch
  :defer nil
  :diminish
  :bind (:map persistent-scratch-mode-map
         ([remap kill-buffer] . (lambda (&rest _)
                                  (interactive)
                                  (user-error "Scratch buffer cannot be killed")))
         ([remap revert-buffer] . persistent-scratch-restore)
         ([remap revert-this-buffer] . persistent-scratch-restore))
  :hook ((after-init . persistent-scratch-autosave-mode)
         (after-init . persistent-scratch-setup-default)
         (lisp-interaction-mode . persistent-scratch-mode))
  :init (setq persistent-scratch-backup-file-name-format "%Y-%m-%d"
              persistent-scratch-backup-directory
              (expand-file-name "persistent-scratch" user-emacs-directory)))


;; A Simple and cool pomodoro timer
(use-package pomidor
  :bind ("s-<f12>" . pomidor)
  :init
  (setq alert-default-style 'mode-line)

  (when *IS-MAC*
    (setq pomidor-play-sound-file
          (lambda (file)
            (when (executable-find "afplay")
              (start-process "pomidor-play-sound" nil "afplay" file))))))


(provide 'init-utils)
