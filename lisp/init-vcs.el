
(use-package transient-posframe
  :diminish
  :defines posframe-border-width
  :custom-face
  (transient-posframe ((t (:inherit tooltip))))
  (transient-posframe-border ((t (:inherit posframe-border :background unspecified))))
  :hook (after-init . transient-posframe-mode)
  :init
  (setq transient-posframe-border-width posframe-border-width
        transient-posframe-min-width 80
        transient-posframe-min-height nil
        transient-posframe-poshandler 'posframe-poshandler-frame-center
        transient-posframe-parameters '((left-fringe . 8)
                                        (right-fringe . 8)))
  :config
  (with-no-warnings
    ;; FIXME:https://github.com/yanghaoxie/transient-posframe/issues/5#issuecomment-1974871665
    (defun my-transient-posframe--show-buffer (buffer _alist)
      "Show BUFFER in posframe and we do not use _ALIST at this period."
      (when (posframe-workable-p)
        (let* ((posframe
                (posframe-show buffer
                               :font transient-posframe-font
                               :position (point)
                               :poshandler transient-posframe-poshandler
                               :background-color (face-attribute 'transient-posframe :background nil t)
                               :foreground-color (face-attribute 'transient-posframe :foreground nil t)
                               :initialize #'transient-posframe--initialize
                               :min-width transient-posframe-min-width
                               :min-height transient-posframe-min-height
                               :internal-border-width transient-posframe-border-width
                               :internal-border-color (face-attribute 'transient-posframe-border :background nil t)
                               :override-parameters transient-posframe-parameters)))
          (frame-selected-window posframe))))
    (advice-add #'transient-posframe--show-buffer :override #'my-transient-posframe--show-buffer)

    (setq transient-mode-line-format nil) ; without line

    (defun transient-posframe--initialize ()
      "Initialize transient posframe."
      (setq window-resize-pixelwise t)
      (setq window-size-fixed nil))

    (defun transient-posframe--resize (window)
      "Resize transient posframe."
      (fit-frame-to-buffer-1 (window-frame window)
                             nil transient-posframe-min-height
                             nil transient-posframe-min-width))
    (advice-add 'transient--fit-window-to-buffer :override #'transient-posframe--resize)

    (defun my-transient-posframe--hide ()
      "Hide transient posframe."
      (posframe-hide transient--buffer-name))
    (advice-add #'transient-posframe--delete :override #'my-transient-posframe--hide)))

(provide 'init-vcs)
