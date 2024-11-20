;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'eglot)
(require 'eglot-java)

;; This is optional. It automatically runs `M-x eglot` for you whenever you are in `elixir-mode`:
(add-hook 'elixir-mode-hook 'eglot-ensure)
(add-hook 'java-mode-hook 'eglot-ensure)
;; Be sure to edit the path appropriately; use the `.bat` script instead for Windows:
(add-to-list 'eglot-server-programs '(elixir-mode "/home/madcomet/codebase/src/elixir-ls-v0.24.1/language_server.sh"))

(setq eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts)
(defun custom-eglot-java-init-opts (server eglot-java-eclipse-jdt)
  "Custom options that will be merged with any default settings."
  '(:settings
    (:java
     (:format
      (:settings
       (:url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
       :enabled t)))))

(add-hook 'java-mode-hook 'eglot-java-mode)
(with-eval-after-load 'eglot-java
  (define-key eglot-java-mode-map (kbd "C-c l n") #'eglot-java-file-new)
  (define-key eglot-java-mode-map (kbd "C-c l x") #'eglot-java-run-main)
  (define-key eglot-java-mode-map (kbd "C-c l t") #'eglot-java-run-test)
  (define-key eglot-java-mode-map (kbd "C-c l N") #'eglot-java-project-new)
  (define-key eglot-java-mode-map (kbd "C-c l T") #'eglot-java-project-build-task)
  (define-key eglot-java-mode-map (kbd "C-c l R") #'eglot-java-project-build-refresh))
;; (add-to-list 'eglot-server-programs '(java-mode "/home/madcomet/codebase/src/jdt-language-server-latest/bin"))

;; ;; java
;; (defconst jdt-jar-path (expand-file-name "jdt-language-server-1.40.0-202409261450/plugins/org.eclipse.equinox.launcher_1.6.900.v20240613-2009.jar" "~/.local/bin"))
;; (defconst jdt-extra-jvm-args '("-noverify"
;; 			       "-javaagent:/Users/jiya/workspace/dotemacs.d/.local/jar/lombok.jar"
;; 			       ;; "-javaagent:[~/.emacs.d/.local/jar/lombok.jar][classes=META-INF/]"
;; 			       "-Xbootclasspath/a:~/.config/emacs/.local/jar/lombok.jar"
;; 			       "--add-modules=ALL-SYSTEM"
;; 			       "--add-opens"
;; 			       "java.base/java.util=ALL-UNNAMED"
;; 			       "--add-opens"
;; 			       "java.base/java.lang=ALL-UNNAMED"
;; 			       ;; "-configuration"
;; 			       ;; "/opt/jdt-language-server/config_mac"
;; 			       ))

;; (defun my-eclipse-jdt-contact (interactive)
;;   "Contact with the jdt server.
;; If INTERACTIVE, prompt user for details."
;;   (let* ((cp (getenv "CLASSPATH"))
;; 	 (contact (unwind-protect (progn
;; 				    (setenv "CLASSPATH" jdt-jar-path)
;; 				    (eglot--eclipse-jdt-contact interactive))
;; 		    (setenv "CLASSPATH" cp)))
;; 	 (jdt-class (car contact))
;; 	 (args (cddr contact)))
;;     (append (list jdt-class "/usr/bin/java")
;; 	    jdt-extra-jvm-args args)))

(provide 'init-eglot)
;;; init-eglot.el ends here
