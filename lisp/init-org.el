;;; init-org.el --- summary -*- lexical-binding: t -*-

;; Author: madcomet
;;; Code:



(use-package org
  :ensure org-contrib
  :ensure valign
  :mode ("\\.org\\'" . org-mode)
  :init
  
  (defun font-exist-p (fontname)
    "Test if this font exists."
    (and fontname (not (string= fontname ""))
         (x-list-fonts fontname)))
  (defun set-font (english chinese size-pair)
    "Setup Emacs English and Chinese fonts."
    (when (font-exist-p english)
      (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))
    (when (font-exist-p chinese)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font t charset (font-spec :family chinese :size (cdr size-pair))))))

  :hook
  ((org-mode . visual-line-mode)
   (org-mode . my/org-prettify-symbols)
   ;; (org-mode . org-indent-mode)
   ;; (org-mode . (lambda () (setq-local line-spacing 0.7)))  ;; 根据需要调整数值: english - 0.3; chinese - 0.7(ai kai)
   (org-mode . toc-org-mode)
   (org-mode . valign-mode)
   ;; (org-mode . variable-pitch-mode)
   (org-babel-after-execute . org-redisplay-inline-images)
   )
  :bind
  (:map org-mode-map
   ("C-c l" . org-store-link)
   ("A-h" . org-mark-element)
   ("C-a" . org-beginning-of-line)
   ("C-e" . org-end-of-line)
   ("C-k" . org-kill-line)
   ("C-x <up>" . org-move-subtree-up)
   ("C-x <down>" . org-move-subtree-down)
   ("C-x f" . org-fold-show-subtree)
   ("C-x i" . org-insert-subheading)
   )
  :commands (org-find-exact-headline-in-buffer
	         org-set-tags) ;;TODO
  ;; :custom-face
  ;; ;; 设置代码块用上下边线包裹
  ;; (org-block-begin-line ((t (:underline t :background unspecified))))
  ;; (org-block-end-line ((t (:overline t :underline nil :background unspecified))))

  :config
  ;; structure templates
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list  'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("rs" . "src rust"))
  (global-set-key (kbd "C-< e") 'tempo-template-org-src-emacs-lisp)
  (global-set-key (kbd "C-< r") 'tempo-template-org-src-rust)

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

  ;; Don't prompt before running code in org
  (setq org-confirm-babel-evaluate nil)
  ;; Display properties
  (setq org-cycle-separator-lines 0)
  (setq org-tags-column 8)
  ;; Set default column view headings: Task Effort Clock_Summary
  (setq org-columns-default-format "%50ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %16TIMESTAMP_IA")
  ;; ;; 设置标题行之间总是有空格；列表之间根据情况自动加空格
  ;; (setq org-blank-before-new-entry '((heading . t)
  ;;                                    (plain-list-item . auto)))
  ;; 在 org mode 里美化字符串
  ;; ================================
  (defun my/org-prettify-symbols ()
    (setq prettify-symbols-alist
          (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                  '(
                    ("[ ]"              . 9744)         ; ☐
                    ("[X]"              . 9745)         ; ☑
                    ("[-]"              . 8863)         ; ⊟
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
    ;; (font-lock-add-keywords
    ;;  'org-mode
    ;;  '(("^ *\\([-]\\) "
    ;;     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
    )

  (add-to-list 'org-emphasis-alist
               '("/" (:foreground "#bb80b3")
                 ))
  (setq
   org-fontify-emphasized-text t)
  (setq
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
  ;; 设置 Org mode 的目录
  (org-directory "~/Dropbox/org")
  (org-log-done t)
  (org-startup-indented t)
  (org-log-into-drawer t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-use-speed-commands
   (lambda ()
     (and (looking-at org-outline-regexp)
          (looking-back "^\**"))))
  ;; 设置笔记的默认存储位置
  (org-default-notes-file (expand-file-name "capture.org" org-directory))
  ;; 启用一些子模块
  (org-modules '(ol-bibtex ol-gnus ol-info ol-eww org-habit org-protocol))

  ;; Syntax hilight in #+begin_src blocks
  (org-src-fontify-natively t)

  (org-src-tab-acts-natively t)

  ;; 一些 Org mode 自带的美化设置
  ;; 标题行美化
  ;; (org-fontify-whole-heading-line t)
  ;; 设置标题行折叠符号
  ;; (org-ellipsis " ▾")
  ;; 在活动区域内的所有标题栏执行某些命令
  (org-loop-over-headlines-in-active-region t)
  ;; TODO 标签美化
  (org-fontify-todo-headline t)
  ;; DONE 标签美化
  (org-fontify-done-headline t)
  ;; 引用块美化
  (org-fontify-quote-and-verse-blocks t)
  ;; ;; 隐藏宏标记
  (org-hide-macro-markers t)
  ;; ;; 隐藏强调标签
  (org-hide-emphasis-markers t)
  ;; 高亮 latex 语法 TODO 有性能问题
  ;; (org-highlight-latex-and-related '(latex native script entities))
  ;; ;; ;; 以 UTF-8 显示
  ;; 显示上下标：x_{2}
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  ;; 是否隐藏标题栏的前置星号，这里我们通过 org-modern 来隐藏
  (org-hide-leading-stars t)
  ;; 当启用缩进模式时自动隐藏前置星号
  (org-indent-mode-turns-on-hiding-stars t)
  ;; 自动启用缩进
  (org-startup-indented t)
  ;; 根据标题栏自动缩进文本
  (org-adapt-indentation t)
  ;; 自动显示图片
  (org-startup-with-inline-images t)
  ;; 默认以 Overview 的模式展示标题行
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
  ;; 设置图片的最大宽度，如果有 imagemagick 支持将会改变图片实际宽度
  ;; 四种设置方法：(1080), 1080, t, nil
  (org-image-actual-width nil)
  ;; imenu 的最大深度，默认为 2
  (org-imenu-depth 4)
  ;; 回车要不要触发链接，这里设置不触发
  (org-return-follows-link nil)
  ;; 上标^下标_是否需要特殊字符包裹，这里设置需要用大括号包裹
  (org-use-sub-superscripts '{})
  ;; 复制粘贴标题行的时候删除 id
  (org-clone-delete-id t)
  ;; 粘贴时调整标题行的级别
  (org-yank-adjusted-subtrees t)

  ;; == Custom State Keywords ==
  ;; TOOD 的关键词设置，可以设置不同的组
  ;; 待办-暂停-进行中-稍后-完成-取消
  (org-todo-keywords '(
                       (sequence "TODO(t)" "|" "DONE(d)" "ABANDON(a)")
                       ))
  ;; Custom colors for the keywords
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ))
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
  (setq org-fast-tag-selection-include-todo t)
  ;; 父子标题栏状态有依赖
  (org-enforce-todo-dependencies t)
  ;; 标题栏和任务复选框有依赖
  ;; (org-enforce-todo-checkbox-dependencies t)
  ;; ;; 优先级样式设置
  ;; (org-priority-faces '((?A :foreground "red")
  ;;                  	    (?B :foreground "orange")
  ;;                  	    (?C :foreground "yellow")))
  ;; ;; ;; 标题行全局属性设置
  (setq org-global-properties '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
        			            ("APPT_WARNTIME_ALL" . "0 5 10 15 20 25 30 45 60")
        			            ("RISK_ALL" . "Low Medium High")
        			            ("STYLE_ALL" . "habit")))
  ;; Org columns 的默认格式
  ;; (org-columns-default-format "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}")
  ;; 当状态从 DONE 改成其他状态时，移除 CLOSED: [timestamp]
  (org-closed-keep-when-no-todo t)
  ;; DONE 时加上时间戳
  (org-log-done 'time)
  ;; 重复执行时加上时间戳
  (org-log-repeat 'time)
  ;; Deadline 修改时加上一条记录
  (org-log-redeadline 'note)
  ;; Schedule 修改时加上一条记录
  (org-log-reschedule 'note)
  ;; 以抽屉的方式记录
  (org-log-into-drawer t)
  ;; 紧接着标题行或者计划/截止时间戳后加上记录抽屉
  (org-log-state-notes-insert-after-drawers nil)

  ;;TODO org-refile 使用缓存

  ;; 设置标签的默认位置，默认是第 77 列右对齐
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
  )
;; NTC org ends
;;config babel languages
;; (with-eval-after-load 'org
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((emacs-lisp . t)
;;      (python . t)
;;      (shell . t)
;;      (rust . t))
;;    )
;;   (push '("conf-unix" . conf-unix) org-src-lang-modes)) ;;TODO what is this?

;; (use-package org-bullets
;;   :after org
;;   :hook (org-mode . org-bullets-mode)
;;   :custom
;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
;;   )

(use-package org-modern
  ;;   :ensure t
  :hook (after-init . (lambda ()
                        (setq org-modern-hide-stars 'leading)
                        (global-org-modern-mode t)))
  :custom
  (org-modern-table nil)
  :config
  ;; 标题行型号字符
  (setq org-modern-star ["◉" "○" "✸" "✳" "◈" "◇" "✿" "❀" "✜"])
  ;; 额外的行间距，0.1 表示 10%，1表示 1px
  ;; tag 边框宽度，还可以设置为 `auto' 即自动计算
  (setq org-modern-label-border 1)
  ;; ;; 设置表格竖线宽度，默认为 3
  ;; (setq org-modern-table-vertical 2)
  ;; ;; 设置表格横线为 0，默认为 0.1
  ;; (setq org-modern-table-horizontal 0)
  ;; 复选框美化
  (setq org-modern-checkbox
        '((?X . #("▢✓" 0 2 (composition ((2)))))
          (?- . #("▢–" 0 2 (composition ((2)))))
          (?\s . #("▢" 0 1 (composition ((1)))))))
  ;; 列表符号美化
  (setq org-modern-list
        '((?- . "•")
          (?+ . "◦")
          (?* . "◉")
          ))
  ;; 代码块左边加上一条竖边线（需要 Org mode 顶头，如果启用了 `visual-fill-column-mode' 会很难看）
  (setq org-modern-block-fringe t)
  ;; 代码块类型美化，我们使用了 `prettify-symbols-mode'
  (setq org-modern-block-name nil)
  ;; #+关键字美化，我们使用了 `prettify-symbols-mode'
  (setq org-modern-keyword nil)
  )

;; (with-eval-after-load 'org (global-org-modern-mode))
;; ;; Add frame borders and window dividers
;; (modify-all-frames-parameters
;;  '((right-divider-width . 40)
;;    (internal-border-width . 40)))
;; (dolist (face '(window-divider
;;                 window-divider-first-pixel
;;                 window-divider-last-pixel))
;;   (face-spec-reset-face face)
;;   (set-face-foreground face (face-attribute 'default :background)))
;; (set-face-background 'fringe (face-attribute 'default :background))

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
;;  org-agenda-tags-column 0
;;  org-ellipsis "…")

;; (global-org-modern-mode)

;; (org-remark-global-tracking-mode +1)

;; ;; Optional if you would like to highlight websites via eww-mode
;; (with-eval-after-load 'eww
;;   (org-remark-eww-mode +1))

;; ;; Optional if you would like to highlight EPUB books via nov.el
;; (with-eval-after-load 'nov
;;   (org-remark-nov-mode +1))

;; ;; Optional if you would like to highlight Info documentation via Info-mode
;; (with-eval-after-load 'info
;;   (org-remark-info-mode +1))

;; (use-package auctex
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq TeX-auto-save t)
;;   (setq TeX-parse-self t)
;;   (setq TeX-PDF-mode t))


;; ;; (require 'setup)
;; ;; ;;;; svg-tag-mode
;; ;; (setup svg-tag-mode
;; ;;   (:after org
;; ;;           (require 'svg-tag-mode))
;; ;;   (:also-load lib-svg-tag-mode)
;; ;;   (:advice
;; ;;    svg-tag-mode-on :around suppress-messages
;; ;;    svg-tag-mode-off :around suppress-messages
;; ;;    svg-tag-make :around eli/svg-tag-with-cache)
;; ;;   (:option*
;; ;;    svg-lib-style-default '(:background "#F5F5F5" :foreground "#37474f" :padding 0.5 :margin 0
;; ;;                                        :stroke 2 :radius 5 :alignment 0.5 :width 20 :height 0.9
;; ;;                                        :scale 0.75 :ascent center :crop-left nil :crop-right nil
;; ;;                                        :collection "material" :font-family "Cascadia Mono"
;; ;;                                        :font-size 11 :font-weight regular)
;; ;;    svg-tag-action-at-point 'edit
;; ;;    svg-lib-icon-collections '(("bootstrap" .
;; ;;                                "https://icons.getbootstrap.com/assets/icons/%s.svg")
;; ;;                               ("simple" .
;; ;;                                "https://raw.githubusercontent.com/simple-icons/simple-icons/develop/icons/%s.svg")
;; ;;                               ("material" .
;; ;;                                "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
;; ;;                               ("octicons" .
;; ;;                                "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
;; ;;                               ("boxicons" .
;; ;;                                "https://boxicons.com/static/img/svg/regular/bx-%s.svg"))
;; ;;    svg-tag-tags `(
;; ;;                   ;; ;; Org tags
;; ;;                   ;; (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
;; ;;                   ;; (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

;; ;;                   ;; Task priority
;; ;;                   ("\\[#[A-Z]\\]" . ( (lambda (tag)
;; ;;                                         (svg-tag-make tag :face 'org-priority
;; ;;                                                       :beg 2 :end -1 :margin 0
;; ;;                                                       :height 1.1 :ascent 16))))

;; ;;                   ;; Progress
;; ;;                   ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
;; ;;                                                       (svg-progress-percent
;; ;;                                                        (substring tag 1 -2)))))
;; ;;                   ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
;; ;;                                                     (svg-progress-count
;; ;;                                                      (substring tag 1 -1)))))

;; ;;                   ;; TODO / DONE
;; ;;                   ("\\(TODO\\)" . ((lambda (tag) (eli/svg-tag-todo-keywords-tag tag 'org-todo))))
;; ;;                   ("\\(NEXT\\)" . ((lambda (tag) (eli/svg-tag-todo-keywords-tag tag 'org-todo))))
;; ;;                   ("\\(STARTED\\)" . ((lambda (tag) (eli/svg-tag-todo-keywords-tag tag 'org-todo))))
;; ;;                   ("\\(SOMEDAY\\)" . ((lambda (tag) (eli/svg-tag-todo-keywords-tag tag 'org-todo))))
;; ;;                   ("\\(PROJECT\\)" . ((lambda (tag) (eli/svg-tag-todo-keywords-tag tag 'org-todo))))
;; ;;                   ("\\(DONE\\)" . ((lambda (tag) (eli/svg-tag-todo-keywords-tag tag 'org-done))))


;; ;;                   ;; Citation of the form [cite:@Knuth:1984]
;; ;;                   ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
;; ;;                                                     (svg-tag-make tag
;; ;;                                                                   :inverse t
;; ;;                                                                   :beg 7 :end -1
;; ;;                                                                   :crop-right t))))
;; ;;                   ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
;; ;;                                                              (svg-tag-make
;; ;;                                                               tag
;; ;;                                                               :end -1
;; ;;                                                               :crop-left t))))

;; ;;                   ;; Active date (with or without day name, with or without time)
;; ;;                   (,(format "\\(<%s>\\)" date-re) .
;; ;;                    ((lambda (tag)
;; ;;                       (svg-tag-make tag :beg 1 :end -1 :margin 0 :ascent 14))))
;; ;;                   (,(format "\\(<%s \\)%s>" date-re day-time-re) .
;; ;;                    ((lambda (tag)
;; ;;                       (svg-tag-make tag :beg 1 :inverse nil :crop-right t
;; ;;                                     :margin 0 :ascent 14))))
;; ;;                   (,(format "<%s \\(%s>\\)" date-re day-time-re) .
;; ;;                    ((lambda (tag)
;; ;;                       (svg-tag-make tag :end -1 :inverse t :crop-left t
;; ;;                                     :margin 0 :ascent 14))))
;; ;;                   ;; Inactive date  (with or without day name, with or without time)
;; ;;                   (,(format "\\(\\[%s\\]\\)" date-re) .
;; ;;                    ((lambda (tag)
;; ;;                       (svg-tag-make tag :beg 1 :end -1 :margin 0
;; ;;                                     :face 'org-date :ascent 14))))
;; ;;                   (,(format "[^ ]\\{6\\}[^-]\\(\\[%s \\)%s\\][^-]" date-re day-time-re) .
;; ;;                    ((lambda (tag)
;; ;;                       (svg-tag-make tag :beg 1 :inverse nil
;; ;;                                     :crop-right t :margin 0 :face 'org-date
;; ;;                                     :ascent 14))))
;; ;;                   (,(format "[^ ]\\{6\\}[^-]\\[%s \\(%s\\]\\)[^-]" date-re day-time-re) .
;; ;;                    ((lambda (tag)
;; ;;                       (svg-tag-make tag :end -1 :inverse t
;; ;;                                     :crop-left t :margin 0 :face 'org-date
;; ;;                                     :ascent 14))))))
;; ;;   (:hooks org-mode-hook (lambda ()
;; ;;                           (make-local-variable 'font-lock-extra-managed-props)
;; ;;                           (svg-tag-mode))
;; ;;           org-agenda-finalize-hook eli/org-agenda-show-svg))

(use-package org-journal
  :ensure t
  :defer nil
  :bind (("C-x j" . org-journal-new-entry))
  :config
  (setq org-journal-dir  "~/Dropbox/Journals/")
  (setq org-journal-date-format   "%F, %A")
  (setq org-journal-time-format  "%T ")
  (setq org-journal-file-format  "%Y-%m-W%V.org")  ; their file names
  (setq org-journal-file-type  'weekly)
  (setq org-journal-enable-agenda-integration  t)
  (setq org-journal-enable-cache  t)

  (defun org-journal-save-entry-and-exit()
    "Simple convenience function.
                Saves the buffer of the current day's entry and kills the window
                Similar to org-capture like behavior"
    (interactive)
    (save-buffer)
    (kill-current-buffer))
  (define-key org-journal-mode-map (kbd "C-x C-s") 'org-journal-save-entry-and-exit)

  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (interactive)
    (org-journal-new-entry t)
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    ))

(use-package org-capture
  :ensure nil
  :bind ("C-c j" . org-capture)
  :hook ((org-capture-mode . (lambda ()
                               (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
         (org-capture-mode . delete-other-windows))
  :custom
  (org-default-notes-file "~/org/inbox.org")
  (org-capture-use-agenda-date nil)
  ;; define common template

  ;; (defvar my-blog-title "")
  (defun input-blog-title ()
    (setq my-blog-title (read-from-minibuffer "Enter blog title: "))
    my-blog-title)

  (defun my/org-capture-new-file-with-date ()
    "Prompt for a new Org file name and return full path with date prefix."
    (let ((title (read-string "New post title: ")))
      (setq my-blog-title title)
      (expand-file-name
       (format "%s-%s.org"
               (format-time-string "%Y-%m-%d")
               (org-hugo-slug title))
       ;; "~/Dropbox/org-roam-files/")))
       "~/Dropbox/org-roam-files/blog-content-org/")))  ;; Adjust this directory as needed


  (org-capture-templates `(
                           ("t" "Task")
                           ("tt" "Task" entry (file+headline "Task.org" "TO-DO Queque")
                            "** TODO %?   %^g"
                            :prepend t
                            :jump-to-captured t)  
                           ("tl" "Lab Tour (It's fun time!)" entry (file+headline "Task.org" "Lab Tour (It's fun time!)")
                            ;; "** TODO %?   %^g"
                            "%(fetch-weather-data)\n"
                            :prepend t
                            :jump-to-captured t)
                           ;; ("tc" "Class-Schedule" entry (file+headline "Task.org" "Class-Schedule")
                           ;;  "* TODO %i%?"
                           ;;  :empty-lines-after
                           ;;  :jump-to-captured t
                           ;;  :prepend t)
                           ("n" "Notes" entry (file+headline "Reading-Summary.org" "Notes")
                            "* %? %^g\n%i\n"
                            :empty-lines-after 1)

                           ("l" "today i learned..." entry (file+headline "Journal.org" "Today i Learned")
                            "* %U - :%?"
                            :empty-lines-after 1
                            :jump-to-captured t
                            :prepend f)

                           ("b" "Blog")
                           ("ba" "Blog Articles" plain (file ,(function my/org-capture-new-file-with-date))
                            ,(concat "* TODO %(progn my-blog-title)  :%^g:@%^{categories}:\n"
                                     ":PROPERTIES:\n"
                                     ":EXPORT_FILE_NAME: %(org-hugo-slug my-blog-title)\n"
                                     ":EXPORT_DATE: " (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)) "\n"
                                     ":ID: %(org-id-new)\n"
                                     ":END:\n"
                                     "%?\n")
                            :window-setup 'other-window
                            :jump-to-captured t)
                           ("bj" "Blog Journal entry" plain (function org-journal-find-location)
                            ,(concat "** %(format-time-string org-journal-time-format)\n"
                                     ":PROPERTIES:\n"
                                     ":EXPORT_FILE_NAME: now - " (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)) "\n"
                                     ":EXPORT_DATE: " (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)) "\n"
                                     ":END:\n"
                                     "%?\n")
                            :window-setup 'other-window
                            :jump-to-captured nil)

                           ("g" "Gallery")
                           ("gf" "字体收藏" entry
                            (file+olp "~/Dropbox/org-roam-files/20250518105542-gallery.org" "字体收藏")
                            "** %^{font-name} :%^g\n- 特点: %^{features}\n- 样例: /样例文字/\n- 链接: %a"
                            :jump-to-captured t)

                           ("gc" "颜色收藏" entry
                            (file+olp "~/Dropbox/20250518105542-gallery.org" "色彩方案")
                            "** %?\n- 色号: \n- 用途: \n#+BEGIN_SRC emacs-lisp\n(setq my-color \"\")\n#+END_SRC")

                           )
                         )
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
  (org-agenda-files '("~/Dropbox/org/Task.org"))
  (org-agenda-sort-notime-is-late nil)
  ;; 时间显示为两位数(9:30 -> 09:30)
  (org-agenda-time-leading-zero t)
  ;; 过滤掉 dynamic
  (org-agenda-hide-tags-regexp (regexp-opt '("dynamic")))
  (org-agenda-compact-blocks t)
  (org-agenda-sticky t)
  (org-agenda-start-on-weekday nil)
  (org-agenda-span 'day)
  (org-agenda-include-diary nil)
  (org-agenda-current-time-string (concat "◀┈┈┈┈┈┈┈┈┈┈┈┈┈ ⏰"))
  (org-agenda-sorting-strategy
   '((agenda habit-down time-up user-defined-up effort-up category-keep)
     (todo category-up effort-up)
     (tags category-up effort-up)
     (search category-up)))
  (org-agenda-window-setup 'current-window)
  (org-agenda-custom-commands
   `(("N" "Notes" tags "NOTE"
      ((org-agenda-overriding-header "Notes")
       (org-tags-match-list-sublevels t)))
     ("g" "GTD"
      ((agenda "" nil)
       (tags-todo "-inbox"
                  ((org-agenda-overriding-header "Next Actions")
                   (org-agenda-tags-todo-honor-ignore-options t)
                   (org-agenda-todo-ignore-scheduled 'future)
                   (org-agenda-skip-function
                    (lambda ()
                      (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                          (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                   (org-tags-match-list-sublevels t)
                   (org-agenda-sorting-strategy
                    '(todo-state-down effort-up category-keep))))
       (tags-todo "-reading/PROJECT"
                  ((org-agenda-overriding-header "Project")
                   (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
                   (org-tags-match-list-sublevels t)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       (tags-todo "+reading/PROJECT"
                  ((org-agenda-overriding-header "Reading")
                   (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
                   (org-tags-match-list-sublevels t)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       (tags-todo "/WAITING"
                  ((org-agenda-overriding-header "Waiting")
                   (org-agenda-tags-todo-honor-ignore-options t)
                   (org-agenda-todo-ignore-scheduled 'future)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       (tags-todo "/DELEGATED"
                  ((org-agenda-overriding-header "Delegated")
                   (org-agenda-tags-todo-honor-ignore-options t)
                   (org-agenda-todo-ignore-scheduled 'future)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       (tags-todo "-inbox"
                  ((org-agenda-overriding-header "On Hold")
                   (org-agenda-skip-function
                    (lambda ()
                      (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                          (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                   (org-tags-match-list-sublevels nil)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       ))
     ("v" "Orphaned Tasks"
      ((agenda "" nil)
       (tags "inbox"
             ((org-agenda-overriding-header "Inbox")
              (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
              (org-tags-match-list-sublevels nil)))
       (tags-todo "+book&-reading/PROJECT"
                  ((org-agenda-overriding-header "Book Plan")
                   (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
                   (org-tags-match-list-sublevels t)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       (tags-todo "-inbox/-NEXT"
                  ((org-agenda-overriding-header "Orphaned Tasks")
                   (org-agenda-tags-todo-honor-ignore-options t)
                   (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
                   (org-agenda-todo-ignore-scheduled 'future)
                   (org-agenda-skip-function
                    (lambda ()
                      (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                          (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                   (org-tags-match-list-sublevels t)
                   (org-agenda-sorting-strategy
                    '(category-keep))))))))
  )

;; (require 'org-habit)

;; ;; (use-package org-super-agenda
;; ;;   :defer nil
;; ;;   :custom
;; ;;   (org-super-agenda-groups '((:auto-dir-name t)))
;; ;;   :config
;; ;;   (org-super-agenda-mode))

;; (use-package org-sidebar :ensure t)



;; (use-package org-alert
;;   :config
;;   )

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
  ;; (require 'org-roam-protocol)
  )

(use-package org-supertag
  :defer nil
  :after org-mode
  :load-path "~/.emacs.d/site-lisp/org-supertag/"
  :hook
  (after-init . org-supertag-config))

(provide 'init-org)
