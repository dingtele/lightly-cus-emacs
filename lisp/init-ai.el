;;; init-ai.el --- summary -*- lexical-binding: t -*-

;; Author: madcomet
;; Maintainer: madcomet
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:


(use-package gptel
  :ensure nil
  :defer nil
  :vc (:url "https://github.com/karthink/gptel" :rev :newest)
  :config
  ;; default backend configuration
  ;; (setq
  ;;  gptel-model "codegeex4:latest"
  ;;  gptel-backend (gptel-make-ollama "Ollama"
  ;;                  :host "localhost:11434"
  ;;                  :stream t
  ;;                  :models '("codegeex4:latest")))

  ;; DeepSeek offers an OpenAI compatible API
  (defun get-openai-api-key ()
    "Return the OpenAI API key from ~/.authinfo."
    (let ((authinfo-file (expand-file-name "~/.authinfo")))
      (with-temp-buffer
        (insert-file-contents authinfo-file)
        (goto-char (point-min))
        (when (re-search-forward "^machine api\\.deepseek\\.com login apikey password \\(\\S-+\\)$" nil t)
          (match-string 1)))))

  (defvar gptel-make-gemini
    (gptel-make-gemini "Gemini"
      :key "AIzaSyCNSfEqa_MS8PQGuJPNVWwfM0ivkuTe7xM"
      :stream t))
  ;; OPTIONAL configuration
  (setq
   gptel-default-mode 'org-mode
   gptel-model 'gemini-2.0-flash
   gptel-backend gptel-make-gemini)


  )

(use-package gptel-quick
  :defer nil
  :after gptel
  :vc (gptel-quick :url "https://github.com/karthink/gptel-quick" :rev :newest)
  :config
  (setq gptel-quick-backend gptel-make-gemini
        gptel-quick-model 'gemini-2.0-flash)
  ;; ;; (keymap-set embark-general-map "?" #'gptel-quick)
  ;; :hook （after-init . gptel-quick）
  )




;; (add-to-list 'load-path "~/.emacs.d/site-lisp/copilot.el-main")
;; (require 'copilot)
;; (add-hook 'prog-mode-hook 'copilot-mode)
;; ;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "M-w") 'copilot-accept-completion-by-word)
;; (define-key copilot-completion-map (kbd "M-q") 'copilot-accept-completion-by-line)

(provide 'init-ai)

;;; init-ai.el ends here
