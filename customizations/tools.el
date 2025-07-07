







(use-package calendar
  :ensure nil
  :hook (calendar-today-visible . calendar-mark-today)
  :custom
  ;; 是否显示中国节日，我们使用 `cal-chinese-x' 插件
  (calendar-chinese-all-holidays-flag nil)
  ;; 是否显示节日
  (calendar-mark-holidays-flag t)
  ;; 是否显示Emacs的日记，我们使用org的日记
  (calendar-mark-diary-entries-flag nil)
  ;; 数字方式显示时区，如 +0800，默认是字符方式如 CST
  (calendar-time-zone-style 'numeric)
  ;; 日期显示方式：year/month/day
  (calendar-date-style 'iso)
  ;; 中文天干地支设置
  (calendar-chinese-celestial-stem ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
  (calendar-chinese-terrestrial-branch ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
  ;; 设置中文月份
  (calendar-month-name-array ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"])
  ;; 设置星期标题显示
  (calendar-day-name-array ["日" "一" "二" "三" "四" "五" "六"])
  ;; 周一作为一周第一天
  (calendar-week-start-day 1)
  )

;; 时间解析增加中文拼音
(use-package parse-time
  :ensure nil
  :defer t
  :config
  (setq parse-time-months
        (append '(("yiy" . 1) ("ery" . 2) ("sany" . 3)
                  ("siy" . 4) ("wuy" . 5) ("liuy" . 6)
                  ("qiy" . 7) ("bay" . 8) ("jiuy" . 9)
                  ("shiy" . 10) ("shiyiy" . 11) ("shiery" . 12)
                  ("yiyue" . 1) ("eryue" . 2) ("sanyue" . 3)
                  ("siyue" . 4) ("wuyue" . 5) ("liuyue" . 6)
                  ("qiyue" . 7) ("bayue" . 8) ("jiuyue" . 9)
                  ("shiyue" . 10) ("shiyiyue" . 11) ("shieryue" . 12))
                parse-time-months))

  (setq parse-time-weekdays
        (append '(("zri" . 0) ("zqi" . 0)
                  ("zyi" . 1) ("zer" . 2) ("zsan" . 3)
                  ("zsi" . 4) ("zwu" . 5) ("zliu" . 6)
                  ("zr" . 0) ("zq" . 0)
                  ("zy" . 1) ("ze" . 2) ("zs" . 3)
                  ("zsi" . 4) ("zw" . 5) ("zl" . 6))
                parse-time-weekdays)))

;; 中国节日设置
(use-package cal-china-x
  :ensure t
  :commands cal-china-x-setup
  :hook (after-init . cal-china-x-setup)
  :config
  ;; 重要节日设置
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  ;; 所有节日设置
  (setq cal-china-x-general-holidays
        '(;;公历节日
          (holiday-fixed 1 1 "元旦")
          (holiday-fixed 2 14 "情人节")
          (holiday-fixed 3 8 "妇女节")
          (holiday-fixed 3 14 "白色情人节")
          (holiday-fixed 4 1 "愚人节")
          (holiday-fixed 5 1 "劳动节")
          (holiday-fixed 5 4 "青年节")
          (holiday-float 5 0 2 "母亲节")
          (holiday-fixed 6 1 "儿童节")
          (holiday-float 6 0 3 "父亲节")
          (holiday-fixed 9 10 "教师节")
          (holiday-fixed 10 1 "国庆节")
          (holiday-fixed 10 2 "国庆节")
          (holiday-fixed 10 3 "国庆节")
          (holiday-fixed 10 24 "程序员节")
          (holiday-fixed 11 11 "双11购物节")
          (holiday-fixed 12 25 "圣诞节")
          ;; 农历节日
          (holiday-lunar 12 30 "春节" 0)
          (holiday-lunar 1 1 "春节" 0)
          (holiday-lunar 1 2 "春节" 0)
          (holiday-lunar 1 15 "元宵节" 0)
          (holiday-solar-term "清明" "清明节")
          (holiday-solar-term "小寒" "小寒")
          (holiday-solar-term "大寒" "大寒")
          (holiday-solar-term "立春" "立春")
          (holiday-solar-term "雨水" "雨水")
          (holiday-solar-term "惊蛰" "惊蛰")
          (holiday-solar-term "春分" "春分")
          (holiday-solar-term "谷雨" "谷雨")
          (holiday-solar-term "立夏" "立夏")
          (holiday-solar-term "小满" "小满")
          (holiday-solar-term "芒种" "芒种")
          (holiday-solar-term "夏至" "夏至")
          (holiday-solar-term "小暑" "小暑")
          (holiday-solar-term "大暑" "大暑")
          (holiday-solar-term "立秋" "立秋")
          (holiday-solar-term "处暑" "处暑")
          (holiday-solar-term "白露" "白露")
          (holiday-solar-term "秋分" "秋分")
          (holiday-solar-term "寒露" "寒露")
          (holiday-solar-term "霜降" "霜降")
          (holiday-solar-term "立冬" "立冬")
          (holiday-solar-term "小雪" "小雪")
          (holiday-solar-term "大雪" "大雪")
          (holiday-solar-term "冬至" "冬至")
          (holiday-lunar 5 5 "端午节" 0)
          (holiday-lunar 8 15 "中秋节" 0)
          (holiday-lunar 7 7 "七夕情人节" 0)
          (holiday-lunar 12 8 "腊八节" 0)
          (holiday-lunar 9 9 "重阳节" 0)))
  ;; 设置日历的节日，通用节日已经包含了所有节日
  (setq calendar-holidays (append cal-china-x-general-holidays)))





;; (require 'svg-tag-mode)
;; (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
;; (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
;; (defconst day-re "[A-Za-z]\\{3\\}")
;; (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

;; ;; (defun svg-progress-percent (value)
;; ;;   (save-match-data
;; ;;     (svg-image (svg-lib-concat
;; ;;                 (svg-lib-progress-bar  (/ (string-to-number value) 100.0)
;; ;;                                        nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
;; ;;                 (svg-lib-tag (concat value "%")
;; ;;                              nil :stroke 0 :margin 0)) :ascent 'center)))

;; ;; (defun svg-progress-count (value)
;; ;;   (save-match-data
;; ;;     (let* ((seq (split-string value "/"))
;; ;;            (count (if (stringp (car seq))
;; ;;                       (float (string-to-number (car seq)))
;; ;;                     0))
;; ;;            (total (if (stringp (cadr seq))
;; ;;                       (float (string-to-number (cadr seq)))
;; ;;                     1000)))
;; ;;       (svg-image (svg-lib-concat
;; ;;                   (svg-lib-progress-bar (/ count total) nil
;; ;;                                         :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
;; ;;                   (svg-lib-tag value nil
;; ;;                                :stroke 0 :margin 0)) :ascent 'center))))

;; (setq svg-tag-tags
;;       `(
;;         ;; Org tags
;;         (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
;;         (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

;;         ;; Task priority
;;         ("\\[#[A-Z]\\]" . ( (lambda (tag)
;;                               (svg-tag-make tag :face 'org-priority
;;                                             :beg 2 :end -1 :margin 0))))

;;         ;; TODO / DONE
;;         ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
;;         ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


;;         ;; Citation of the form [cite:@Knuth:1984]
;;         ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
;;                                           (svg-tag-make tag
;;                                                         :inverse t
;;                                                         :beg 7 :end -1
;;                                                         :crop-right t))))
;;         ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
;;                                                    (svg-tag-make tag
;;                                                                  :end -1
;;                                                                  :crop-left t))))


;;         ;; Active date (with or without day name, with or without time)
;;         (,(format "\\(<%s>\\)" date-re) .
;;          ((lambda (tag)
;;             (svg-tag-make tag :beg 1 :end -1 :margin 0))))
;;         (,(format "\\(<%s \\)%s>" date-re day-time-re) .
;;          ((lambda (tag)
;;             (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
;;         (,(format "<%s \\(%s>\\)" date-re day-time-re) .
;;          ((lambda (tag)
;;             (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

;;         ;; Inactive date  (with or without day name, with or without time)
;;         (,(format "\\(\\[%s\\]\\)" date-re) .
;;          ((lambda (tag)
;;             (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
;;         (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
;;          ((lambda (tag)
;;             (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
;;         (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
;;          ((lambda (tag)
;;             (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

;;         ;; ;; Progress
;;         ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
;;                                             (svg-progress-percent (substring tag 1 -2)))))
;;         ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
;;                                           (svg-progress-count (substring tag 1 -1)))))
;;         ))
;; (global-svg-tag-mode 1)

;;      (defun my/update-tab-bar-after-theme-change (&rest _args)
;;        "Update tab bar face attributes after a theme change."
;;        (set-face-attribute 'tab-bar-tab nil
;;                            :inherit 'doom-modeline-panel
;;                            :foreground 'unspecified
;;                            :background 'unspecified)
;;        (set-face-attribute 'tab-bar nil
;;                            :foreground (face-attribute 'default :foreground)))

;;      (advice-add 'load-theme :after #'my/update-tab-bar-after-theme-change)
;;      (my/update-tab-bar-after-theme-change)


;;     (require 'svg-lib)
;;      (require 'svg-tag-mode)
;;      (require 'lib-svg-tag-mode)
;;      (require 'lib-tab-bar)




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

(use-package tab-line
  :ensure nil
  ;; :hook (after-init . global-tab-line-mode)
  :custom
  (tab-line-tabs-function 'tab-line-tabs-window-buffers)
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

  ;; (my/set-tab-theme)

  ;;(dolist (mode '(ediff-mode process-menu-mode term-mode vterm-mode))
  ;;(add-to-list 'tab-line-exclude-modes mode))
  (dolist (mode '(ediff-mode process-menu-mode))
    (add-to-list 'tab-line-exclude-modes mode))

  )


;; (use-package eglot
;;   :defer t
;;   :hook
;;   ;; (python-ts-mode . eglot-ensure)
;;   ;; (clojure-mode . eglot-ensure)
;;   ;; (clojure-ts-mode . eglot-ensure)
;;   ;; (clojure-ts-clojurescript-mode .eglot-ensure)
;;   (eglot-managed-mode . #'my/eglot-capf)

;;   :config
;;   (add-to-list 'eglot-server-programs '(python-mode . ("pyright")))
;;   (add-to-list 'eglot-server-programs '(clojure-mode . ("clojure-lsp")))
;;   (add-to-list 'eglot-server-programs '(clojure-ts-mode . ("clojure-lsp")))
;;   (add-to-list 'eglot-server-programs '(clojure-ts-clojurescript-mode . ("clojure-lsp")))

;;   (defun my/eglot-capf ()
;;     "Set custom completion-at-point functions for Eglot."
;;     (setq-local completion-at-point-functions '(eglot-completion-at-point)))
;;   )

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

;; (require 'eglot)

;;   (require 'eglot-java)
;;   (add-hook 'java-mode-hook #'eglot-java-mode)
;;   (setq eglot-java-server-install-dir "~/codebase/src/java/eclipse.jdt.ls")
;;   (setq eglot-java-eclipse-jdt-cache-directory "~/tmp/eglot-eclipse-jdt-cache")

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

(require 'dash)
(use-package origami :ensure t :defer t)
  (with-eval-after-load 'origami
    (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
    (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes))

;; (use-package hideshow-org
;;   :vc (:url "https://github.com/shanecelis/hideshow-org"))
;; (global-set-key (kbd"C-c h") 'hs-org/minor-mode)

;; (use-package lsp-brigde
;;   :vc (:url "https://github.com/manateelazycat/lsp-bridge")
;;   :config
;;   (global-lsp-bridge-mode))

;; (require 'yasnippet)
;; (yas-global-mode 1)

(use-package esup
  :ensure t
:defer t
  :commands esup)



(use-package rg :defer t)



;; ;; Enable desired features for all lisp modes
;; (require 'clojure-ts-mode)
;; (setq clojure-ts-grammar-recipes nil)

;; (require 'clojure-mode)
;; (setq clojure-indent-style 'always-indent
;;       clojure-indent-keyword-style 'always-indent
;;       clojure-enable-indent-specs nil)

;; (push '(clojure-mode . clojure-ts-mode) major-mode-remap-alist)
;; (push '(clojurescript-mode . clojure-ts-clojurescript-mode) major-mode-remap-alist)

;; (require 'cljsbuild-mode)
;; (require 'elein)

;; (add-hook 'after-save-hook #'check-parens nil t)

;; (require 'cider)
;; (setq nrepl-popup-stacktraces nil)
;; (add-hook 'clojure-ts-mode-hook #'cider-mode)

;; (with-eval-after-load 'cider
;;   (add-hook 'cider-repl-mode-hook 'subword-mode))

;; (require 'flycheck-clojure)
;; (with-eval-after-load 'clojure-ts-mode
;;     (with-eval-after-load 'cider
;;       (with-eval-after-load 'flycheck
;;         (flycheck-clojure-setup))))

;; (unless (package-installed-p 'inf-clojure)
;;   (package-refresh-contents)
;;   (package-install 'inf-clojure))
;; (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

;; Emacs lisp mode
(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
         ("C-c C-x" . ielm)
         ("C-c C-c" . eval-defun)
         ("C-c C-b" . eval-buffer))
  :config
  ;; Syntax highlighting of known Elisp symbols
  (use-package highlight-defined
    :hook ((emacs-lisp-mode inferior-emacs-lisp-mode) . highlight-defined-mode))

  (with-no-warnings
    ;; Align indent keywords
    ;; @see https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
    (defun my-lisp-indent-function (indent-point state)
      "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

 `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

 an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

 a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
      (let ((normal-indent (current-column))
            (orig-point (point)))
        (goto-char (1+ (elt state 1)))
        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
        (cond
         ;; car of form doesn't seem to be a symbol, or is a keyword
         ((and (elt state 2)
               (or (not (looking-at "\\sw\\|\\s_"))
                   (looking-at ":")))
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
         ((and (save-excursion
                 (goto-char indent-point)
                 (skip-syntax-forward " ")
                 (not (looking-at ":")))
               (save-excursion
                 (goto-char orig-point)
                 (looking-at ":")))
          (save-excursion
            (goto-char (+ 2 (elt state 1)))
            (current-column)))
         (t
          (let ((function (buffer-substring (point)
                                            (progn (forward-sexp 1) (point))))
                method)
            (setq method (or (function-get (intern-soft function)
                                           'lisp-indent-function)
                             (get (intern-soft function) 'lisp-indent-hook)))
            (cond ((or (eq method 'defun)
                       (and (null method)
                            (length> function 3)
                            (string-match "\\`def" function)))
                   (lisp-indent-defform state indent-point))
                  ((integerp method)
                   (lisp-indent-specform method state
                                         indent-point normal-indent))
                  (method
                   (funcall method indent-point state))))))))
    (add-hook 'emacs-lisp-mode-hook
              (lambda () (setq-local lisp-indent-function #'my-lisp-indent-function)))

    ;; Add remove buttons for advices
    (add-hook 'help-mode-hook 'cursor-sensor-mode)

    (defun function-advices (function)
      "Return FUNCTION's advices."
      (let ((flist (indirect-function function)) advices)
        (while (advice--p flist)
          (setq advices `(,@advices ,(advice--car flist)))
          (setq flist (advice--cdr flist)))
        advices))

    (defun add-remove-advice-button (advice function)
      (when (and (functionp advice) (functionp function))
        (let ((inhibit-read-only t)
              (msg (format "Remove advice `%s'" advice)))
          (insert "\t")
          (insert-button
           "Remove"
           'face 'custom-button
           'cursor-sensor-functions `((lambda (&rest _) ,msg))
           'help-echo msg
           'action (lambda (_)
                     (when (yes-or-no-p msg)
                       (message "%s from function `%s'" msg function)
                       (advice-remove function advice)
                       (if (eq major-mode 'helpful-mode)
                           (helpful-update)
                         (revert-buffer nil t))))
           'follow-link t))))

    (defun add-button-to-remove-advice (buffer-or-name function)
      "Add a button to remove advice."
      (with-current-buffer buffer-or-name
        (save-excursion
          (goto-char (point-min))
          (let ((ad-list (function-advices function)))
            (while (re-search-forward "^\\(?:This function has \\)?:[-a-z]+ advice: \\(.+\\)$" nil t)
              (let ((advice (car ad-list)))
                (add-remove-advice-button advice function)
                (setq ad-list (delq advice ad-list))))))))

    (define-advice describe-function-1 (:after (function) advice-remove-button)
      (add-button-to-remove-advice (help-buffer) function))
    (with-eval-after-load 'helpful
      (define-advice helpful-update (:after () advice-remove-button)
        (when helpful--callable-p
          (add-button-to-remove-advice (current-buffer) helpful--sym))))

    ;; Remove hooks
    (defun remove-hook-at-point ()
      "Remove the hook at the point in the *Help* buffer."
      (interactive)
      (unless (memq major-mode '(help-mode helpful-mode))
        (error "Only for help-mode or helpful-mode"))

      (let ((orig-point (point)))
        (save-excursion
          (when-let*
              ((hook (progn (goto-char (point-min)) (symbol-at-point)))
               (func (when (and
                            (or (re-search-forward (format "^Value:?[\s|\n]") nil t)
                                (goto-char orig-point))
                            (sexp-at-point))
                       (end-of-sexp)
                       (backward-char 1)
                       (catch 'break
                         (while t
                           (condition-case _err
                               (backward-sexp)
                             (scan-error (throw 'break nil)))
                           (let ((bounds (bounds-of-thing-at-point 'sexp)))
                             (when (<= (car bounds) orig-point (cdr bounds))
                               (throw 'break (sexp-at-point)))))))))
            (when (yes-or-no-p (format "Remove %s from %s? " func hook))
              (remove-hook hook func)
              (if (eq major-mode 'helpful-mode)
                  (helpful-update)
                (revert-buffer nil t)))))))
    (bind-key "r" #'remove-hook-at-point help-mode-map)))

;; Interactive macro expander
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
         ("C-c e" . macrostep-expand)
         :map lisp-interaction-mode-map
         ("C-c e" . macrostep-expand)))

(setq tab-always-indent 'complete)
  (setq python-indent-offset 4)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max)))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
                                        ;(setq user-emacs-directory "~/.cache/emacs")

;; (use-package no-littering)

;; ;; no-littering doesn't set this by default so we must place
;; ;; auto save files in the same path as it uses for sessions
;; (setq auto-save-file-name-transforms
;;       `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))


(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


;; set up for programming languages
;; (require 'highlight-indent-guides)
;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;; (set-face-background 'highlight-indent-guides-odd-face "darkgray")
;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
;; (setq highlight-indent-guides-method 'character)


(use-package plantuml
            :load-path "~/.emacs.d/site-lisp/plantuml-emacs/")
(setq plantuml-jar-path "~/.emacs.d/site-lisp/plantuml-1.2024.6.jar"
      plantuml-output-type "svg"
      plantuml-relative-path "/home/madcomet/Pictures/plantuml-image/"
      plantuml-theme "plain"
      plantuml-font "source code pro medium"
      plantuml-add-index-number t
      plantuml-log-command t
      plantuml-mindmap-contains-org-content t
      plantuml-org-headline-bold t)

(use-package eshell-syntax-highlighting
  :after eshell-mode
  :ensure t ;; Install if not already installed.
  :hook
  ;; Enable in all Eshell buffers.
  (eshell-mode . eshell-syntax-highlighting-global-mode))


(windmove-default-keybindings 'control)


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
    :hook
    (after-init . smooth-scrolling-mode))

  ;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (custom-set-variables '(ad-redefinition-action (quote accept)))

  (bind-key "M-`" 'other-frame)

   ;; 将原本放在 .emacs.d 目录下的一些配置信息或动态信息，转移到 etc 或 var 子目录里，让配置目录更加简洁清爽
   (use-package no-littering
     :ensure t :defer t)

(defvar latitude "31.30883235990429")
(defvar longitude "120.73078133558143")

(defun fetch-weather-data (&rest _)
  "Fetch weather data from API and return weather string."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&daily=weather_code,temperature_2m_max,temperature_2m_min,sunrise,sunset,uv_index_max&timezone=Asia%%2FSingapore&forecast_days=1" latitude longitude)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let* ((json-data (buffer-substring-no-properties (point) (point-max)))
             (json-obj (json-read-from-string json-data))
             (daily (cdr (assoc 'daily json-obj)))
             (weather-code (aref (cdr (assoc 'weather_code daily)) 0))
             (temp-max (aref (cdr (assoc 'temperature_2m_max daily)) 0))
             (temp-min (aref (cdr (assoc 'temperature_2m_min daily)) 0))
             (sunrise (aref (cdr (assoc 'sunrise daily)) 0))
             (sunset (aref (cdr (assoc 'sunset daily)) 0))
             (uv (uv-to-sunscreen-advice (aref (cdr (assoc 'uv_index_max daily)) 0)))
             (weather-description (weather-code-to-string weather-code))
             (weather-string (format "** Weather: %s\n*** Temperature: %.1f°C-%.1f°C\n*** Daytime: %s-%s\n*** UV: %s"
                                     weather-description temp-min temp-max sunrise sunset uv)))
        weather-string))))

(defun uv-to-sunscreen-advice (uv-index)
  "Return sunscreen advice based on the given UV index."
  (let ((uv-str (number-to-string uv-index)))
    (cond
     ((<= uv-index 2) (concat uv-str " 通常不需要特别防护，但可以考虑使用 SPF 15 的防晒霜。"))
     ((<= uv-index 5) (concat uv-str " 建议使用 SPF 15-30 的防晒霜，尤其是在户外活动时。"))
     ((<= uv-index 7) (concat uv-str " 建议使用 SPF 30-50 的防晒霜，并采取其他防护措施，如戴帽子和太阳镜。"))
     ((<= uv-index 10) (concat uv-str " 建议使用 SPF 50+的防晒霜，并尽量避免在阳光最强的时段外出，同时采取其他防护措施。"))
     ((>= uv-index 11) (concat uv-str " 强烈建议使用 SPF 50+的防晒霜，并采取一切可能的防护措施，如穿长袖衣物、戴帽子和太阳镜，尽量避免暴露在阳光下。"))
     (t "输入的 UV 指数无效。"))))

(defun weather-code-to-string (code)
  "Convert weather CODE to a human-readable string."
  (cond
   ((= code 0) "Clear sky")
   ((= code 1) "Mainly clear")
   ((= code 2) "Partly cloudy")
   ((= code 3) "Overcast")
   ((= code 45) "Fog")
   ((= code 48) "Depositing rime fog")
   ((= code 51) "Drizzle: Light")
   ((= code 53) "Drizzle: Moderate")
   ((= code 55) "Drizzle: Dense intensity")
   ((= code 56) "Freezing Drizzle: Light")
   ((= code 57) "Freezing Drizzle: Dense intensity")
   ((= code 61) "Rain: Slight")
   ((= code 63) "Rain: Moderate")
   ((= code 65) "Rain: Heavy intensity")
   ((= code 66) "Freezing Rain: Light")
   ((= code 67) "Freezing Rain: Heavy intensity")
   ((= code 71) "Snow fall: Slight")
   ((= code 73) "Snow fall: Moderate")
   ((= code 75) "Snow fall: Heavy intensity")
   ((= code 77) "Snow grains")
   ((= code 80) "Rain showers: Slight")
   ((= code 81) "Rain showers: Moderate")
   ((= code 82) "Rain showers: Violent")
   ((= code 85) "Snow showers: Slight")
   ((= code 86) "Snow showers: Heavy")
   ((= code 95) "Thunderstorm: Slight or moderate")
   ((= code 96) "Thunderstorm with slight hail")
   ((= code 99) "Thunderstorm with heavy hail")
   (t "Unknown weather condition")))
