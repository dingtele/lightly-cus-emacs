(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t)
  (setq org-appear-autokeywords t)
  (setq org-appear-inside-latex t)
  )

(use-package org-block-capf
  :ensure t
  :vc (:url https://github.com/xenodium/org-block-capf))
(require 'org-block-capf)
(add-hook 'org-mode-hook #'org-block-capf-add-to-completion-at-point-functions)

(use-package org
  :ensure t  :ensure org-contrib
  :mode ("\\.org\\'" . org-mode)
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . my/org-prettify-symbols)
   ;; (org-mode . org-indent-mode)
   (org-mode . toc-org-mode)
   (org-mode . variable-pitch-mode)
   (org-mode . my-nov-font-setup))
  ;; :diminish visual-line-mode
  ;; :diminish org-indent-mode
  ;; :defer t
  :bind (("\C-c a" . org-agenda)
         ("\C-c c" . org-capture))
  :commands (org-find-exact-headline-in-buffer
	     org-set-tags) ;;TODO
  :custom-face
  ;; 设置Org mode标题以及每级标题行的大小
  (org-document-title ((t (:height 1.75 :weight bold))))
  (org-level-1 ((t (:height 1.2 :weight bold))))
  (org-level-2 ((t (:height 1.15 :weight bold))))
  (org-level-3 ((t (:height 1.1 :weight bold))))
  (org-level-4 ((t (:height 1.05 :weight bold))))
  (org-level-5 ((t (:height 1.0 :weight bold))))
  (org-level-6 ((t (:height 1.0 :weight bold))))
  (org-level-7 ((t (:height 1.0 :weight bold))))
  (org-level-8 ((t (:height 1.0 :weight bold))))
  (org-level-9 ((t (:height 1.0 :weight bold))))
  ;; 设置代码块用上下边线包裹
  (org-block-begin-line ((t (:underline t :background unspecified))))
  (org-block-end-line ((t (:overline t :underline nil :background unspecified))))

  :config
  ;; structure templates
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (global-set-key (kbd "C-< s") 'tempo-template-org-src-emacs-lisp)

  ;; (defun +diminish-org-indent ()
  ;;   "Diminish org-indent-mode on the modeline"
  ;;   (interactive)
  ;;   (diminish 'org-indent-mode ""))
  ;; (add-hook 'org-indent-mode-hook #'+diminish-org-indent)

  ;; Fix evil-auto-indent for org buffers.
  ;; (defun gs-org-disable-evil-auto-indent nil
  ;;   "Disables evil's auto-indent for org."
  ;;   (setq evil-auto-indent nil)
  ;; )
  ;; (add-hook 'org-mode-hook #'gs-org-disable-evil-auto-indent) ;;TODO
  (setq org-startup-with-inline-images t)

  ;; Syntax hilight in #+begin_src blocks
  (setq org-src-fontify-natively t)
  ;; Don't prompt before running code in org
  (setq org-confirm-babel-evaluate nil)
  ;; Display properties
  (setq org-cycle-separator-lines 0)
  (setq org-tags-column 8)
  ;; Show overview when open
  (setq org-startup-folded t)
  ;; Set default column view headings: Task Effort Clock_Summary
  (setq org-columns-default-format "%50ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %16TIMESTAMP_IA")
  ;; ;; 设置标题行之间总是有空格；列表之间根据情况自动加空格
  ;; (setq org-blank-before-new-entry '((heading . t)
  ;;                                    (plain-list-item . auto)))
  ;; 在org mode里美化字符串
  ;; ================================
  (defun my/org-prettify-symbols ()
    (setq prettify-symbols-alist
          (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                  '(
                    ;; ("[ ]"              . 9744)         ; ☐
                    ;; ("[X]"              . 9745)         ; ☑
                    ;; ("[-]"              . 8863)         ; ⊟
                    ("#+begin_src"      . 9998)         ; ✎
                    ("#+end_src"        . 9633)         ; □
                    ("#+begin_example"  . 129083)       ; 🠻
                    ("#+end_example"    . 129081)       ; 🠹
                    ("#+results:"       . 9776)         ; ☰
                    ("#+attr_latex:"    . "🄛")
                    ("#+attr_html:"     . "🄗")
                    ("#+attr_org:"      . "🄞")
                    ("#+name:"          . "🄝")         ; 127261
                    ("#+caption:"       . "🄒")         ; 127250
                    ("#+date:"          . "📅")         ; 128197
                    ("#+author:"        . "💁")         ; 128100
                    ("#+setupfile:"     . 128221)       ; 📝
                    ("#+email:"         . 128231)       ; 📧
                    ("#+startup:"       . 10034)        ; ✲
                    ("#+options:"       . 9965)         ; ⛭
                    ("#+title:"         . 10162)        ; ➲
                    ("#+subtitle:"      . 11146)        ; ⮊
                    ("#+downloaded://///:"    . 8650)         ; ⇊
                    ("#+language:"      . 128441)       ; 🖹
                    ("#+begin_quote"    . 187)          ; »
                    ("#+end_quote"      . 171)          ; «
                    ("#+begin_results"  . 8943)         ; ⋯
                    ("#+end_results"    . 8943)         ; ⋯
                    )))
    (setq prettify-symbols-unprettify-at-point t)
    (prettify-symbols-mode 1)
    )

  (add-to-list 'org-emphasis-alist
               '("/" (:foreground "#bb80b3")
                 ))
  (setq
   org-fontify-emphasized-text t)
  (setq
   ;;  ;; Edit settings
   ;;  org-auto-align-tags nil
   ;;  org-tags-column 0
   ;;  org-catch-invisible-edits 'show-and-error
   ;;  org-special-ctrl-a/e t
   ;;  org-insert-heading-respect-content t

   ;;  ;; Org styling, hide markup etc.
   ;;  ;;org-hide-emphasis-markers t
   ;;  ;;org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  ;; ;; Ellipsis styling
  ;; (setq org-ellipsis "…")
  ;; (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
  :custom
  ;; 设置Org mode的目录
  (org-directory "~/Dropbox/org")
  ;; 设置笔记的默认存储位置
  (org-default-notes-file (expand-file-name "capture.org" org-directory))
  ;; 启用一些子模块
  (org-modules '(ol-bibtex ol-gnus ol-info ol-eww org-habit org-protocol))

  ;; 一些Org mode自带的美化设置
  ;; 标题行美化
  ;; (org-fontify-whole-heading-line t)	
  ;; 设置标题行折叠符号
  ;; (org-ellipsis " ▾")
  ;; 在活动区域内的所有标题栏执行某些命令
  (org-loop-over-headlines-in-active-region t)
  ;; TODO标签美化
  (org-fontify-todo-headline t)
  ;; DONE标签美化
  (org-fontify-done-headline t)
  ;; 引用块美化
  (org-fontify-quote-and-verse-blocks t) 
  ;; ;; 隐藏宏标记
  (org-hide-macro-markers t)
  ;; ;; 隐藏强调标签
  (org-hide-emphasis-markers t)
  ;; 高亮latex语法 TODO 有性能问题
  ;; (org-highlight-latex-and-related '(latex native script entities)) 
  ;; ;; ;; 以UTF-8显示
  ;; 显示上下标：x_{2}
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  ;; 是否隐藏标题栏的前置星号，这里我们通过org-modern来隐藏
  (org-hide-leading-stars t)
  ;; 当启用缩进模式时自动隐藏前置星号
  (org-indent-mode-turns-on-hiding-stars t)
  ;; 自动启用缩进
  (org-startup-indented t)
  ;; 根据标题栏自动缩进文本
  (org-adapt-indentation t)
  ;; 自动显示图片
  (org-startup-with-inline-images t)
  ;; 默认以Overview的模式展示标题行
  (org-startup-folded 'overview)
  ;; 允许字母列表
  (org-list-allow-alphabetical t)
  ;; 列表的下一级设置
  (org-list-demote-modify-bullet '(
                       		   ("-"  . "+")
                                   ("+"  . "1.")
                       		   ("1." . "a.")
                       		   ))
  ;; 编辑时检查是否在折叠的不可见区域
  (org-fold-catch-invisible-edits 'smart)
  ;; 在当前位置插入新标题行还是在当前标题行后插入，这里设置为当前位置
  (org-insert-heading-respect-content nil)
  ;; 设置图片的最大宽度，如果有imagemagick支持将会改变图片实际宽度
  ;; 四种设置方法：(1080), 1080, t, nil
  (org-image-actual-width nil)
  ;; imenu的最大深度，默认为2
  (org-imenu-depth 4)
  ;; 回车要不要触发链接，这里设置不触发
  (org-return-follows-link nil)
  ;; 上标^下标_是否需要特殊字符包裹，这里设置需要用大括号包裹
  (org-use-sub-superscripts '{})
  ;; 复制粘贴标题行的时候删除id
  (org-clone-delete-id t)
  ;; 粘贴时调整标题行的级别
  (org-yank-adjusted-subtrees t)

  ;; == Custom State Keywords ==
  ;; TOOD的关键词设置，可以设置不同的组
  ;; 待办-暂停-进行中-稍后-完成-取消
  (org-todo-keywords '((sequence "TODO(t)" "HOLD(h!)" "WIP(i!)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
		       (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f!)")))
  ;; Custom colors for the keywords
  (setq org-todo-keyword-faces
        '(
          ("TODO" :foreground "red" :weight bold)
          ("HOLD" :foreground "#5C888B" :weight bold)
          ("WIP" :foreground "blue" :weight bold)
          ;; ("PROJ" :foreground "magenta" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ("WAIT" :foreground "orange" :weight bold)
          ;; 	("INACTIVE" :foreground "magenta" :weight bold)
          ;; 	("SOMEDAY" :foreground "cyan" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)))
  ;; 当标题行状态变化时标签同步发生的变化
  ;; Moving a task to CANCELLED adds a CANCELLED tag
  ;; Moving a task to WAIT adds a WAIT tag
  ;; Moving a task to HOLD adds WAIT and HOLD tags
  ;; Moving a task to a done state removes WAIT and HOLD tags
  ;; Moving a task to TODO removes WAIT, CANCELLED, and HOLD tags
  ;; Moving a task to DONE removes WAIT, CANCELLED, and HOLD tags
  (org-todo-state-tags-triggers
   (quote (("CANCELLED" ("CANCELLED" . t))
           ("WAIT" ("WAIT" . t))
           ("HOLD" ("WAIT") ("HOLD" . t))
           (done ("WAIT") ("HOLD"))
           ("TODO" ("WAIT") ("CANCELLED") ("HOLD"))
           ("DONE" ("WAIT") ("CANCELLED") ("HOLD")))))

  ;; 使用专家模式选择标题栏状态
  (org-use-fast-todo-selection 'expert)
  ;; Allow setting single tags without the menu
  ;; (setq org-fast-tag-selection-single-key 'expert)
  ;; Include the todo keywords
  ;; (setq org-fast-tag-selection-include-todo t)
  ;; 父子标题栏状态有依赖
  (org-enforce-todo-dependencies t)
  ;; 标题栏和任务复选框有依赖
  (org-enforce-todo-checkbox-dependencies t)
  ;; 优先级样式设置
  ;; (org-priority-faces '((?A :foreground "red")
  ;;                  	(?B :foreground "orange")
  ;;                  	(?C :foreground "yellow")))
  ;; ;; 标题行全局属性设置
  ;; (org-global-properties '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
  ;;       					   ("APPT_WARNTIME_ALL" . "0 5 10 15 20 25 30 45 60")
  ;;       					   ("RISK_ALL" . "Low Medium High")
  ;;       					   ("STYLE_ALL" . "habit")))
  ;; Org columns的默认格式
  ;; (org-columns-default-format "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}")
  ;; 当状态从DONE改成其他状态时，移除 CLOSED: [timestamp]
  (org-closed-keep-when-no-todo t)
  ;; DONE时加上时间戳
  (org-log-done 'time)
  ;; 重复执行时加上时间戳
  (org-log-repeat 'time)
  ;; Deadline修改时加上一条记录
  (org-log-redeadline 'note)
  ;; Schedule修改时加上一条记录
  (org-log-reschedule 'note)
  ;; 以抽屉的方式记录
  (org-log-into-drawer t)
  ;; 紧接着标题行或者计划/截止时间戳后加上记录抽屉
  (org-log-state-notes-insert-after-drawers nil)

  ;;TODO org-refile 使用缓存

  ;; 设置标签的默认位置，默认是第77列右对齐
  (org-tags-column -77)
  ;; 自动对齐标签
  (org-auto-align-tags t)
  ;; 标签不继承
  (org-use-tag-inheritance nil)
  ;; 在日程视图的标签不继承
  (org-agenda-use-tag-inheritance nil)
  ;; 标签快速选择
  (org-use-fast-tag-selection t)
  ;; 标签选择不需要回车确认
  (org-fast-tag-selection-single-key t)
  ;; 定义了有序属性的标题行也加上 OREDERD 标签
  (org-track-ordered-property-with-tag t)
  ;; 始终存在的的标签
  (org-tag-persistent-alist '(("read"     . ?r)
			      ("emacs"    . ?e)
			      ("study"    . ?s)
			      ("work"     . ?w)))
  ;; 预定义好的标签
  (org-tag-alist '((:startgroup)
                   ("linux"    . ?l)
                   ("apple"    . ?a)
                   ("noexport" . ?n)
                   ("ignore"   . ?i)
                   ("toc"      . ?t)
                   (:endgroup)))

  ;; 归档设置
  (org-archive-location "%s_archive::datetree/")
  (defun org-archive-done-tasks ()
    "Archive all tasks marked DONE in the file."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree))
     "/DONE" 'file))

  (use-package cdlatex
    :ensure t)
  (with-eval-after-load 'org
    (require 'cdlatex)
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex))
  )

;;config babel languages
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)))
  (push '("conf-unix" . conf-unix) org-src-lang-modes));;TODO what is this?

(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/org/inbox.org")

(use-package org-capture
  :ensure nil
  :bind ("\e\e c" . (lambda () (interactive) (org-capture)))
  :hook ((org-capture-mode . (lambda ()
                               (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
         (org-capture-mode . delete-other-windows))
  :custom
  (org-capture-use-agenda-date nil)
  ;; define common template
  (org-capture-templates `(
                           ("t" "Task")
                           ("tt" "Task" entry (file+headline "Task.org" "TO-DO Queque")
                            "** TODO %?   %^g"
                            :prepend t
                            :jump-to-captured t)
                           ("tp" "Weekly-emacs-plugin" entry (file+headline "Task.org" "Weekly-Emacs-Plugin")
                            "** TODO %?   %^g"
                            :prepend t
                            :jump-to-captured t)
                           ("tc" "Class-Schedule" entry (file+headline "Task.org" "Class-Schedule")
                            "* TODO %i%?"
                            :empty-lines-after
                            :jump-to-captured t
                            :prepend t)
                           ("n" "Notes" entry (file+headline "Reading-Summary.org" "Notes")
                            "* %? %^g\n%i\n"
                            :empty-lines-after 1)
                           ;; For EWW
                           ;; ("b" "Bookmarks" entry (file+headline "capture.org" "Bookmarks")
                           ;;  "* %:description\n\n%a%?"
                           ;;  :empty-lines 1
                           ;;  :immediate-finish t)
                           ;; ("j" "Journal")
                           ;; ("jt" "Today's TODO" entry (file+olp+datetree "Journal.org" "Today's TODO")
                           ;;  "* TODO %U [/] \n - [ ] %?"
                           ;;  :empty-lines 1
                           ;;  :jump-to-captured t
                           ;;  :prepend f)
                           ("l" "today i learned..." entry (file+olp+datetree "Journal.org")
                            "* %U - :%?"
                            :empty-lines-after 1
                            :prepend f)
                           ("w" "Web site" entry
                            (file "")
                            "* %a :website:\n\n%U %?\n\n%:initial")
                           ))
  )

(use-package org-agenda
  :ensure nil
  :after org
  :bind
  ("C-c a" . org-agenda)
  :custom
  (org-agenda-include-diary t)
  (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                              ;; Indent todo items by level to show nesting
                              (todo . " %i %-12:c%l")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))
  (org-agenda-start-on-weekday nil)
  (custom-set-variables '(org-agenda-files
                          '("~/Dropbox/org/Task.org")))
  )

(require 'org-habit)

(use-package org-super-agenda
  :defer nil
  :custom
  (org-super-agenda-groups '((:auto-dir-name t)))
  :config
  (org-super-agenda-mode))

(use-package org-sidebar :ensure t)

(use-package org-journal
  :config
  (setq org-journal-dir  "~/Dropbox/org/")
  (setq org-journal-date-format   "%F, %A")
  (setq org-journal-time-format  "%T ")
  (setq org-journal-file-format  "%Y.org")  ; their file names
  (setq org-journal-file-type  'yearly)
  (setq org-journal-enable-agenda-integration  t)
  (setq org-journal-enable-cache  t)

  (defun org-journal-save-entry-and-exit()
    "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
    (interactive)
    (save-buffer)
    (kill-buffer-and-window))
  (define-key org-journal-mode-map (kbd "C-x C-s") 'org-journal-save-entry-and-exit))

(use-package org-alert
  :config
  )

(use-package org-zettel-ref-mode
  :ensure nil
  :vc (:url "https://github.com/yibie/org-zettel-ref-mode" :rev :newest)
  ;; :load-path "~/.emacs.d/site-lisp/org-zettel-ref-mode/"
  :init
  (setq org-zettel-ref-overview-directory "~/Dropbox/Notes")
  :config
  ;; (setq org-zettel-ref-mode-type 'denote)
  (setq org-zettel-ref-mode-type 'org-roam)
  ;; (setq org-zettel-ref-mode-type 'normal)
  (setq org-zettel-ref-python-file "~/.emacs.d/elpa/org-zettel-ref-mode/convert-to-org.py")
  (setq org-zettel-ref-temp-folder "~/Dropbox/book-store/to-be-converted/")
  (setq org-zettel-ref-reference-folder "~/Dropbox/book-store/converted-org")
  (setq org-zettel-ref-archive-folder "~/Dropbox/book-store/archives/")
  (setq org-zettel-ref-python-environment 'venv)
  (setq org-zettel-ref-python-env-name "venv")
  (setq org-zettel-ref-debug t)
  (setq org-zettel-ref-highlight-types
        (append org-zettel-ref-highlight-types
                '(("warning" . (:char "w"
                                      :face (:background "#FFA726"
                                                         :foreground "#000000"
                                                         :extend t)
                                      :name "warning"
                                      :prefix "⚠️"))
                  ("success" . (:char "s"
                                      :face (:background "#66BB6A"
                                                         :foreground "#FFFFFF"
                                                         :extend t)
                                      :name "success"
                                      :prefix "✅")))))
  (define-key org-zettel-ref-minor-mode-map (kbd "C-c q") 'org-zettel-ref-add-quick-note)
  (define-key org-zettel-ref-minor-mode-map (kbd "C-c p") 'org-zettel-ref-quick-markup)
  )

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

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Dropbox/org-roam-files/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;; (require 'org-modern)
;; ;; Option 1: Per buffer
;; (add-hook 'org-mode-hook #'org-modern-mode)
;; (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; (setq
;;  ;; Edit settings
;;  org-auto-align-tags nil
;;  org-tags-column 0
;;  org-catch-invisible-edits 'show-and-error
;;  org-special-ctrl-a/e t
;;  org-insert-heading-respect-content t

;;  ;; Org styling, hide markup etc.
;;  org-hide-emphasis-markers t
;;  org-pretty-entities t

;;  ;; Agenda styling
;;  org-agenda-tags-column 0
;;  org-agenda-block-separator ?─
;;  org-agenda-time-grid
;;  '((daily today require-timed)
;;    (800 1000 1200 1400 1600 1800 2000)
;;    " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
;;  org-agenda-current-time-string
;;  "◀── now ─────────────────────────────────────────────────")

;; ;; Ellipsis styling
;; ;; (setq org-ellipsis "…")
;; (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

;; (use-package org-modern
;;   :ensure t
;;   :hook (after-init . (lambda ()
;;                         (setq org-modern-hide-stars 'leading)
;;                         (global-org-modern-mode t)))
;;   :config
;;   ;; 标题行型号字符
;;   ;; (setq org-modern-star ["◉" "○" "✸" "✳" "◈" "◇" "✿" "❀" "✜"])
;;   ;; 额外的行间距，0.1表示10%，1表示1px
;;   (setq-default line-spacing 0.1)
;;   ;; tag边框宽度，还可以设置为 `auto' 即自动计算
;;   (setq org-modern-label-border 1)
;;   ;; 设置表格竖线宽度，默认为3
;;   (setq org-modern-table-vertical 2)
;;   ;; 设置表格横线为0，默认为0.1
;;   (setq org-modern-table-horizontal 0)
;;   ;; 复选框美化
;;   (setq org-modern-checkbox
;;         '((?X . #("▢✓" 0 2 (composition ((2)))))
;;           (?- . #("▢–" 0 2 (composition ((2)))))
;;           (?\s . #("▢" 0 1 (composition ((1)))))))
;;   ;; 列表符号美化
;;   (setq org-modern-list
;;         '((?- . "•")
;;           (?+ . "◦")
;;           ;; (?* . "▹")
;; ))
;;   ;; 代码块左边加上一条竖边线（需要Org mode顶头，如果启用了 `visual-fill-column-mode' 会很难看）
;;   (setq org-modern-block-fringe t)
;;   ;; 代码块类型美化，我们使用了 `prettify-symbols-mode'
;;   (setq org-modern-block-name nil)
;;   ;; #+关键字美化，我们使用了 `prettify-symbols-mode'
;;   (setq org-modern-keyword nil)
;;   )

(org-remark-global-tracking-mode +1)

;; Optional if you would like to highlight websites via eww-mode
(with-eval-after-load 'eww
  (org-remark-eww-mode +1))

;; Optional if you would like to highlight EPUB books via nov.el
(with-eval-after-load 'nov
  (org-remark-nov-mode +1))

;; Optional if you would like to highlight Info documentation via Info-mode
(with-eval-after-load 'info
  (org-remark-info-mode +1))

(use-package auctex
  :ensure t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t))



(provide 'init-org)
