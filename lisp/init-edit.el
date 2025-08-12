;;; init-edit.el --- summary   -*- lexical-binding: t -*-

;; Author: madcomet
;; Homepage: homepage
;; Keywords: keywords


;;; Commentary:

;;; Code:


(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; reStore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))


(use-package vundo
  :bind ("C-x u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols))

;; auto completion of function name/path/file name
(bind-key "C-<tab>" 'hippie-expand)
;; (global-set-key "\M- " 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)
(set-variable 'tab-width 8)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(use-package saveplace
  :defer nil
  :config
  (save-place-mode)
  (setq save-place-file (concat user-emacs-directory "places")))

;; History
(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(delete-selection-mode t)

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;;disable electri
(setq electric-indent-mode nil)

;;scroll down multiple lines
(defun previous-multilines ()
  (interactive)
  (scroll-down (/ (window-body-height) 3)))

(defun next-multilines ()
  "scroll up multiple lines"
  (interactive)
  (scroll-up (/ (window-body-height) 3)))

(global-set-key "\M-n" 'next-multilines)
(global-set-key "\M-p" 'previous-multilines)
;; Move line up
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

;; Move line down
(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

;; Assign the custom keybindings
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(global-set-key (kbd "M-o") 'other-window)
(windmove-default-keybindings)

(global-set-key (kbd "C-x g") 'magit-status)
;; Shift lines up and down withM-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.


  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-define-key '("j" . next-line))
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     ))
(require 'meow)
  (meow-setup)
  (meow-global-mode 1)
  ;; disable meow mode in magit-mode
  (defun my/no-meow-in-term ()
    (when (derived-mode-p 'magit-mode)
      (meow-mode -1)))
  (add-hook 'after-change-major-mode-hook 'my/no-meow-in-term)

  ;; example: create a new keyboard layout - state
  (setq meow-reading-keymap (make-keymap))
  (meow-define-state reading
    "meow state for interacting with smartparens"
    :lighter " [📖]"
    :keymap meow-reading-keymap)

  ;; meow-define-state creates the variable
  (setq meow-cursor-type-reading 'hollow)

  (meow-define-keys 'reading
    '("<escape>" . meow-normal-mode)
    '("d" . bing-dict-brief)
    '("c" . +anki-helper-capture-cloze-card)
    '("u" . meow-undo))

  



  (use-package browse-url
    :ensure nil
    :defines dired-mode-map
    :bind (("C-c C-z ." . browse-url-at-point)
           ("C-c C-z b" . browse-url-of-buffer)
           ("C-c C-z r" . browse-url-of-region)
           ("C-c C-z u" . browse-url)
           ("C-c C-z e" . browse-url-emacs)
           ("C-c C-z v" . browse-url-of-file))
    :init
    (with-eval-after-load 'dired
      (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map)))

  ;; Click to browse URL or to send to e-mail address
  (use-package goto-addr
    :ensure nil
    :hook ((text-mode . goto-address-mode)
           (prog-mode . goto-address-prog-mode)
           (org-mode . goto-address-mode)))

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :defines desktop-minor-mode-table
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

;; Redefine M-< and M-> for some modes
(use-package beginend
  :diminish beginend-global-mode
  :hook (after-init . beginend-global-mode)
  :config (mapc (lambda (pair)
                  (diminish (cdr pair)))
                beginend-modes))

(use-package rime
  :defer nil
  :commands (toggle-input-method)
  :hook
  (minibuffer-setup . rime-mode)
  ((meow-insert-enter . (lambda() (when (derived-mode-p 'org-mode 'telega-chat-mode)
                                    (set-input-method "rime"))))
   (meow-insert-exit . (lambda() (set-input-method nil))))
  :bind
  (:map rime-mode-map
   ("C-j" . rime-inline-ascii)
   ("C-`" . 'rime-send-keybinding)
   ("C-l" . rime-force-enable))
  :custom
  ;; 默认值
  (rime-translate-keybindings
   '("C-S 3" "C-S 4" "C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>" "<ctl>" "<shift-l>"))
  (default-input-method 'rime)
  (rime-show-candidate 'posframe)
  (rime-posframe-style 'vertical)
  (rime-posframe-properties 
   (list :background-color "#333333"
         :foreground-color "#dcdccc"
         :internal-border-width 1))
  (rime-disable-predicates
   '(rime-predicate-prog-in-code-p

     rime-predicate-auto-english-p

     rime-predicate-punctuation-after-ascii-p
     rime-predicate-punctuation-line-begin-p
     my/rime-predicate-punctuation-next-char-is-paired-p
     rime-predicate-tex-math-or-command-p
     rime-predicate-org-latex-mode-p
     rime-predicate-current-uppercase-letter-p
     (lambda () (button-at (point)))
     meow-normal-mode-p
     meow-motion-mode-p
     meow-keypad-mode-p
     ;; +rime--punctuation-line-begin-p
     ;; +rime--english-prober
     ;; If the cursor is after a alphabet character.
     rime-predicate-after-alphabet-char-p
     ;; If input a punctuation after
     ;; a Chinese charactor with whitespace.
     rime-predicate-punctuation-after-space-cc-p
     rime-predicate-special-ascii-line-begin-p
     ))
  (rime-inline-predicates
   ;; If cursor is after a whitespace
   ;; which follow a non-ascii character.
   '(rime-predicate-space-after-cc-p
     ;; If the current charactor entered is a uppercase letter.
     rime-predicate-current-uppercase-letter-p))

  (rime-user-data-dir "~/.emacs.d/rime/")
  (when *IS-MAC*
    (rime-librime-root "~/.emacs.d/librime/dist")
    (rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/")
    )


  ;; (rime-inline-ascii-trigger 'shift-l);; keycode for communicating with rime config,not for users.

  :init
  (defun my/rime-predicate-punctuation-next-char-is-paired-p ()
    (if (not (eq (point) (point-max)))
        (and (rime-predicate-current-input-punctuation-p)
             (not (string-match-p
                   (rx (any "\"\(\[\{"))
                   (buffer-substring (point) (1- (point)))
                   )
                  )
             (string-match-p
              (rx (any "\}\]\)\""))
              (buffer-substring (point) (1+ (point)))))
      nil))

  (defun rime-predicate-special-ascii-line-begin-p ()
    "If '/' or '#' at the beginning of the line."
    (and (> (point) (save-excursion (back-to-indentation) (point)))
         (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
           (string-match-p "^[\/#]" string))))

  )


(use-package pangu-spacing
  :hook (after-init . global-pangu-spacing-mode)
  :custom
  (pangu-spacing-real-insert-separtor t))

;; 快速打开配置文件
(defun open-init-file-and-eval()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  (eval-buffer))
(global-set-key (kbd "<f1>") 'open-init-file-and-eval)

(use-package clipetty
  :ensure t
  :defer t
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



(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :pretty-hydra
  ((:title (pretty-hydra-title "HideShow" 'octicon "nf-oct-fold")
    :color amaranth :quit-key ("q" "C-g"))
   ("Fold"
    (("t" hs-toggle-all "toggle all")
     ("a" hs-show-all "show all")
     ("i" hs-hide-all "hide all")
     ("g" hs-toggle-hiding "toggle hiding")
     ("c" hs-cycle "cycle block")
     ("s" hs-show-block "show block")
     ("h" hs-hide-block "hide block")
     ("l" hs-hide-level "hide level"))
    "Move"
    (("C-a" mwim-beginning-of-code-or-line "⭰")
     ("C-e" mwim-end-of-code-or-line "⭲")
     ("C-b" backward-char "←")
     ("C-n" next-line "↓")
     ("C-p" previous-line "↑")
     ("C-f" forward-char "→")
     ("C-v" pager-page-down "↘")
     ("M-v" pager-page-up "↖")
     ("M-<" beginning-of-buffer "⭶")
     ("M->" end-of-buffer "⭸"))))
  :bind (:map hs-minor-mode-map
         ("C-~" . hideshow-hydra/body)
         ("C-S-<escape>" . hideshow-hydra/body))
  :hook (prog-mode . hs-minor-mode)
  :config
  ;; More functions
  ;; @see https://karthinks.com/software/simple-folding-with-hideshow/
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-toggle-all ()
    "Toggle hide/show all."
    (interactive)
    (pcase last-command
      ('hs-toggle-all
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; Display line counts
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (concat
                    " "
                    (propertize
                     (if (char-displayable-p ?⏷) "⏷" "...")
                     'face 'shadow)
                    (propertize
                     (format " (%d lines)"
                             (count-lines (overlay-start ov)
                                          (overlay-end ov)))
                     'face '(:inherit shadow :height 0.8))
                    " "))))
  (setq hs-set-up-overlay #'hs-display-code-line-counts))


(provide 'init-edit)

;;; init-edit.el ends her
