;;; init-package.el --- summary -*- lexical-binding: t -*-

;; Author: madcomet
;; Homepage: homepage
;; Keywords: keywords


;;; Commentary:

;; commentary

;;; Code:




(require 'package)

;; Emacs Lisp Package Archive (ELPA)
;; @see https://github.com/melpa/melpa and https://elpa.emacs-china.org/.
(defcustom centaur-package-archives-alist
  (let ((proto (if (gnutls-available-p) "https" "http")))
    `((melpa    . (("gnu"    . ,(format "%s://elpa.gnu.org/packages/" proto))
                   ("nongnu" . ,(format "%s://elpa.nongnu.org/nongnu/" proto))
                   ("melpa"  . ,(format "%s://melpa.org/packages/" proto))))
      (bfsu     . (("gnu"    . ,(format "%s://mirrors.bfsu.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.bfsu.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.bfsu.edu.cn/elpa/melpa/" proto))))
      (iscas    . (("gnu"    . ,(format "%s://mirror.iscas.ac.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirror.iscas.ac.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirror.iscas.ac.cn/elpa/melpa/" proto))))
      (netease  . (("gnu"    . ,(format "%s://mirrors.163.com/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.163.com/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.163.com/elpa/melpa/" proto))))
      (sjtu     . (("gnu"    . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/" proto))))
      (tuna     . (("gnu"    . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/" proto))))
      (ustc     . (("gnu"    . ,(format "%s://mirrors.ustc.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.ustc.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.ustc.edu.cn/elpa/melpa/" proto))))))
  "A list of the package archives."
  :group 'centaur
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))

(defcustom centaur-package-archives 'melpa
  "Set package archives from which to fetch."
  :group 'centaur
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value centaur-package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    centaur-package-archives-alist)))


(defun set-package-archives (archives &optional refresh async no-save)
  "Set the package ARCHIVES (ELPA).

REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background.
Save to option `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern
     (completing-read "Select package archives: "
                      (mapcar #'car centaur-package-archives-alist)))))
  ;; Set option
  (custom-set-variables '(centaur-archives-package archives))

  ;; Refresh if need
  (and refresh (package-refresh-contents async))

  (message "Set package archives to `%s'" archives))

;; Refer to https://emacs-china.org/t/elpa/11192
(defun centaur-test-package-archives (&optional no-chart)
  "Test connection speed of all package archives and display on chart.

Not displaying the chart if NO-CHART is non-nil.
Return the fastest package archive."
  (interactive)

  (let* ((durations (mapcar
                     (lambda (pair)
                       (let ((url (concat (cdr (nth 2 (cdr pair)))
                                          "archive-contents"))
                             (start (current-time)))
                         (message "Fetching %s..." url)
                         (ignore-errors
                           (url-copy-file url null-device t))
                         (float-time (time-subtract (current-time) start))))
                     centaur-package-archives-alist))
         (fastest (car (nth (cl-position (apply #'min durations) durations)
                            centaur-package-archives-alist))))

    ;; Display on chart
    (when (and (not no-chart)
               (require 'chart nil t)
               (require 'url nil t))
      (chart-bar-quickie
       'vertical
       "Speed test for the ELPA mirrors"
       (mapcar (lambda (p) (symbol-name (car p))) centaur-package-archives-alist)
       "ELPA"
       (mapcar (lambda (d) (* 1e3 d)) durations) "ms"))

    (message "`%s' is the fastest package archive" fastest)

    ;; Return the fastest
    fastest))

;; Required by `use-package'
(use-package diminish :ensure t)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; Update packages
;; (use-package auto-package-update
;;   :init
;;   (setq auto-package-update-delete-old-versions t
;;         auto-package-update-hide-results t
;;         auto-package-update-interval 7)
;;   (auto-package-update-maybe))

(use-package auto-compile
  :defer nil
  :config (auto-compile-on-load-mode))

;; optimize for build-in :vc to avoid long time deep clone with all package's history
(defun my/vc-git-clone (fn remote directory rev)
  (if (or (not (string-match-p "elpa" directory))
          (null rev))
      (funcall fn remote directory rev)
    (cond
     ((ignore-errors
        ;; First try if rev is a branch/tag name
        ;; https://stackoverflow.com/a/48748567/2163429
        (vc-git--out-ok "clone" "--depth" "1" "--single-branch" "--branch" rev remote directory)))
     ((vc-git--out-ok "clone" "--single-branch" remote directory)
      (let ((default-directory directory))
        (vc-git--out-ok "checkout" rev))))
    directory))

(advice-add 'vc-git-clone :around
            'my/vc-git-clone)

;; install the required packages
;; Set missing package vars
(defvar lem-missing-packages '()
  "List populated at startup containing packages needing installation.")
(defvar lem-missing-vc-packages '()
  "List populated at startup containing vc packages requiring installation.")

;; Check for packages
(defun lem-check-missing-packages ()
  "Check for missing packages."
  (interactive)
  ;; Check packages
  (message "%s" "Checking for missing packages.")
  (dolist (p package-selected-packages)
    (unless (package-installed-p p)
      (add-to-list 'lem-missing-packages p 'append)))
  ;; Check vc installed packages (Emacs 29+)
  (when (version< "29" emacs-version)
    (message "%s" "Checking for missing vc packages.")
    (dolist (p package-vc-selected-packages)
      (unless (package-installed-p (car p))
        (add-to-list 'lem-missing-vc-packages (car p) 'append)))))

;; Install packages
(defun lem-install-missing-packages ()
  "Install missing packages from package & package-vc lists."
  (interactive)
  (lem-check-missing-packages)
  (cond ((or lem-missing-packages
             lem-missing-vc-packages)
         (message "Refreshing package database & installing missing packages...")
         (package-install-selected-packages t)
         (setq lem-missing-packages '())
         (package-vc-install-selected-packages)
         (setq lem-missing-vc-packages '()))
        (t
         (message "No missing packages."))))

(provide 'init-package)

;;; init-package.el ends here
