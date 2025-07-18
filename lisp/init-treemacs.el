
;; A tree layout file explorer
(use-package treemacs
  :defer nil
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-git-mode)
  :hook (after-init . treemacs)
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  :bind (([f8]        . treemacs)
         ("M-0"       . treemacs-select-window)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t b"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
         ("C-x t d"   . treemacs-select-directory)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-width                   30
        ;; treemacs-no-png-images           (not centaur-icon)
        )
  (setq treemacs-position 'left
        treemacs-display-in-side-window t)


  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (use-package treemacs-nerd-icons
    :demand t
    ;; :when (icons-displayable-p)
    :custom-face
    (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.6))))
    (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
    :config (treemacs-load-theme "nerd-icons"))

  (use-package treemacs-magit
    :hook ((magit-post-commit
            git-commit-post-finish
            magit-post-stage
            magit-post-unstage)
           . treemacs-magit--schedule-update))

  (use-package treemacs-tab-bar
    :demand t
    :config (treemacs-set-scope-type 'Tabs)))
;; (treemacs-start-on-boot)

(provide 'init-treemacs)
