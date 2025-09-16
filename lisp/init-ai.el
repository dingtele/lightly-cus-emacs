;;; init-ai.el --- summary -*- lexical-binding: t -*-

;; Author: madcomet
;;; Code:

(use-package gptel
  :defer nil
  :vc (:url "https://github.com/karthink/gptel" :rev :newest)  
  :config
  ;; DeepSeek offers an OpenAI compatible API
  (defun get-deepseek-api-key ()
    "Return the OpenAI API key from ~/.authinfo."
    (let ((authinfo-file (expand-file-name "~/Dropbox/.authinfo")))
      (with-temp-buffer
        (insert-file-contents authinfo-file)
        (goto-char (point-min))
        (when (re-search-forward "^machine api\\.deepseek\\.com login apikey password \\(\\S-+\\)$" nil t)
          (match-string 1)))))
  
  (defun get-gemini-api-key ()
    "Return the OpenAI API key from ~/.authinfo."
    (let ((authinfo-file (expand-file-name "~/Dropbox/.authinfo")))
      (with-temp-buffer
        (insert-file-contents authinfo-file)
        (goto-char (point-min))
        (when (re-search-forward "^machine generativelanguage\\.googleapis\\.com login apikey password \\(\\S-+\\)$" nil t)
          (match-string 1)))))

  (setq
   gptel-default-mode 'org-mode
   gptel-model 'gemini-2.0-flash
   gptel-backend (gptel-make-gemini "Gemini"
                   :key 'get-gemini-api-key
                   :stream t))
  )

(use-package gptel-quick
  :after gptel
  :vc (gptel-quick :url "https://github.com/karthink/gptel-quick" :rev :newest)
  ;; :config
  ;; ;; (keymap-set embark-general-map "?" #'gptel-quick)
  :hook (after-init . gptel-quick))


(use-package superchat
  :vc (:url "https://github.com/yibie/superchat" :rev :newest)
  :init
  ;; Set the data storage directory
  (setq superchat-data-directory "~/.emacs.d/superchat/")

  ;; Set the session save directory
  (setq superchat-save-directory "~/.emacs.d/superchat/chat-notes/")

  ;; Set default directories for file selection
  (setq superchat-default-directories '("~/Documents" )))


(use-package ragmacs
  :vc (:url "https://github.com/positron-solutions/ragmacs.git")
  :after gptel
  :defer
  :init
  ;; (setq gptel-tools
  ;;       (list ragmacs-manuals 
  ;;             ragmacs-symbol-manual-node
  ;;             ragmacs-manual-node-contents
  ;;             ragmacs-function-source
  ;;             ragmacs-variable-source))
  (gptel-make-preset 'introspect
    :pre (lambda () (require 'ragmacs))
    :system
    "You are pair programming with the user in Emacs and on Emacs.
 
 Your job is to dive into Elisp code and understand the APIs and
 structure of elisp libraries and Emacs.  Use the provided tools to do
 so, but do not make duplicate tool calls for information already
 available in the chat.
 
 <tone>
 1. Be terse and to the point.  Speak directly.
 2. Explain your reasoning.
 3. Do NOT hedge or qualify.
 4. If you don't know, say you don't know.
 5. Do not offer unprompted advice or clarifications.
 6. Never apologize.
 7. Do NOT summarize your answers.
 </tone>
 
 <code_generation>
 When generating code:
 1. Always check that functions or variables you use in your code exist.
 2. Also check their calling convention and function-arity before you use them.
 3. Write code that can be tested by evaluation, and offer to evaluate
 code using the `elisp_eval` tool.
 </code_generation>
 
 <formatting>
 1. When referring to code symbols (variables, functions, tags etc) enclose them in markdown quotes.
    Examples: `read_file`, `getResponse(url, callback)`
    Example: `<details>...</details>`
H 2. If you use LaTeX notation, enclose math in \( and \), or \[ and \] delimiters.
 </formatting>"
    :tools '("introspection")))


;; (add-to-list 'load-path "~/.emacs.d/site-lisp/copilot.el-main")
;; (require 'copilot)
;; (add-hook 'prog-mode-hook 'copilot-mode)
;; ;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "M-w") 'copilot-accept-completion-by-word)
;; (define-key copilot-completion-map (kbd "M-q") 'copilot-accept-completion-by-line)

(provide 'init-ai)

;;; init-ai.el ends here
