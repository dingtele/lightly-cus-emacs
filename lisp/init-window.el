;;; init-window.el --- summary -*- lexical-binding: t -*-

;; Author: madcomet
;;; Code:

(use-package shackle
  :ensure t
  :defer nil
  :custom
  (shackle-lighter "")
  (shackle-select-reused-windows nil) ; default nil
  (shackle-default-alignment 'below) ; default below
  (shackle-default-size 0.4) ; default 0.5
  ;; (shackle-default-rule '(:select t))
  (shackle-rules
   ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
   '((compilation-mode              :select nil                                                            )
     ("*undo-tree*"                 :size 0.25                         :align right)
     ("*Shell Command Output*"      :select nil                                               )
     ("\\*Async Shell.*\\*"                      :regexp t :ignore t                          )
     (occur-mode                    :select nil                        :align t :size 0.3)
     ("*Help*"                      :select t  :align right :size 0.3 :popup t)
     ;; (help-mode :select t :align right :size 0.3 :popup t)
     (helpful-mode                  :select t                                      :align right)
     ("*Completions*"                                                  :size 0.3  :align t    )
     ("*Messages*"                  :select nil :inhibit-window-quit nil :align below :size 0.3)
     ("\\*[Wo]*Man.*\\*"  :regexp t :select t   :inhibit-window-quit t :other t               ) ;TODO ?
     ("\\*poporg.*\\*"    :regexp t :select t                          :other t               ) ;TODO ?
     ("*Calendar*"                  :select t                          :size 0.3  )
     ("*info*"                      :select t   :inhibit-window-quit t  :same t)
     (magit-status-mode             :select t   :inhibit-window-quit t :same t)
     (magit-log-mode                :select t   :inhibit-window-quit t :same t)
     ;; ("*Capture*" :select t :inhibit-window-quit nil :size 0.3 :align right)q
     ;; (org-capture-mode :select t :inhibit-window-quit nil :align right :size 0.4)
     ("*Packages*" :select t :same t)
     (pdf-outline-buffer-mode :select t :align 'below)
     ("*eshell*" :select t :align below :size 0.3 :popup t)
     ("*Gemini*" :select t  :inhibit-window-quit t  :same t :align right :size 0.4)
     ;; (treemacs-mode                 :select t :inhibit-window-quit t :align left :size 0.4 :popup t)
     ))
  :hook
  (after-init . shackle-mode))

(use-package popper
  :ensure t
  :bind (
         ;; ("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "\\*Async Shell Command\\*"
     help-mode
     occur-mode
     eshell-mode
     "^\\*eshell.*\\*$" eshell-mode 
     "^\\*shell.*\\*$"  shell-mode
     ;; ("\\*corfu\\*" . hide)
     (compilation-mode . hide)
     ibuffer-mode
     debugger-mode           
     magit-status-mode
     "*Gemini*"
     ))
  ;; group by project.el, projectile, directory or perspective
  (popper-group-function nil)
  ;; pop in child frame or not
  (popper-display-function #'display-buffer-in-child-frame)
  ;; use `shackle.el' to control popup
  (popper-display-control nil)

  :hook 
  (after-init . popper-mode)
  (after-init . popper-echo-mode)

  :config    
  ;; HACK: close popper window with `C-g'
  (defun +popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add #'keyboard-quit-dwim :before #'+popper-close-window-hack)
  )







(provide 'init-window)
;;; name.el ends here
