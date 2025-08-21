;;; gptel-watch.el --- Auto call gptel-request based on trigger patterns -*- lexical-binding: t; -*-

;; Author: ISouthRain
;; Version: 0.2.1
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))
;; Keywords: AI, convenience
;; URL: https://github.com/ISouthRain/gptel-watch

;; Copyright (C) 2025 Free Software Foundation, Inc.
;; License: GPL-3.0-or-later

;;; Commentary:

;; `gptel-watch-mode` is a minor mode that automatically invokes `gptel-request`
;; when the user finishes typing a line that ends with certain trigger patterns
;; (e.g., "AI!", "#ai", etc.). It extracts context around the line,
;; clears the line content, and sends it to a large language model (LLM).
;;
;; This allows seamless in-buffer assistance from GPT models by marking intent inline.

;;; Code:

(require 'gptel)
(require 'gptel-rewrite)
(require 'cl-lib)

(defgroup gptel-watch nil
  "Automatic GPT requests triggered by buffer text patterns."
  :group 'convenience
  :prefix "gptel-watch-")

(defcustom gptel-watch-trigger-patterns '("AI" "AI!" "#ai" "ai")
  "List of line-ending patterns that trigger `gptel-watch-mode` actions."
  :type '(repeat regexp)
  :group 'gptel-watch)

(defcustom gptel-watch-context-lines 10
  "Number of lines before and after the trigger line to include as context."
  :type 'integer
  :group 'gptel-watch)

(defcustom gptel-watch-trigger-commands '(newline org-return)
  "Commands that trigger GPT context extraction in `gptel-watch-mode`."
  :type '(repeat (function :tag "Command"))
  :group 'gptel-watch)

(defcustom gptel-watch-system-prompt
  "你作为一个文本助手, 拥有写作和编程能力.
你根据上下文推测意图, 帮我编写内容.
比如我发送:
int main()
{
  // 打印 Hello World. AI!
}
然后你根据上下文推测 文本 AI 这行用意, 然后你返回内容.
仅仅返回你写的内容, 比如:
printf(\"Hello World\");

下面是限制你返回内容的条件:
简洁回复.
请不要发送任何 Markdown 格式代码:
```language
Code
```
请不要发送任何 Markdown 格式代码.
请不要发送任何 Markdown 格式代码.
"
  "System prompt passed to `gptel-request`."
  :type 'string
  :group 'gptel-watch)

;;;###autoload
(defun gptel-watch ()
  "Manually invoke GPT context generation on current line if it matches any trigger."
  (interactive)
  (when (gptel-watch--line-matches-p)
    (gptel-watch--handle-request)))

(defun gptel-watch--log (fmt &rest args)
  "Internal logging utility for gptel-watch."
  (apply #'message (concat "[gptel-watch] " fmt) args))

(defun gptel-watch--line-matches-p ()
  "Return non-nil if the current line ends with a trigger pattern."
  (let ((line (thing-at-point 'line t)))
    (when line
      (cl-some (lambda (pat) (string-match-p (concat pat "$") line))
               gptel-watch-trigger-patterns))))

(defun gptel-watch--extract-context ()
  "Extract surrounding lines as context based on `gptel-watch-context-lines`."
  (let ((start (save-excursion
                 (forward-line (- gptel-watch-context-lines))
                 (line-beginning-position)))
        (end (save-excursion
               (forward-line gptel-watch-context-lines)
               (line-end-position))))
    (buffer-substring-no-properties start end)))

(defun gptel-watch--clear-line ()
  "Clear current line (preserving newline)."
  (delete-region (line-beginning-position) (line-end-position)))

(defun gptel-watch--handle-request ()
  "Send extracted context to GPT and clear current line."
  (let ((context (gptel-watch--extract-context)))
    (gptel-watch--clear-line)
    (gptel-watch--log "Sending context to GPT.")
    (gptel-request context :system gptel-watch-system-prompt)))

(defun gptel-watch--handle-request ()
  "Send extracted context to GPT and show diff overlay with the result."
  (let ((context (gptel-watch--extract-context))
        (beg (line-beginning-position))
        (end (line-end-position)))
    ;; (gptel-watch--clear-line) ;; Delete the current line.
    (gptel-watch--log "Sending context to GPT.")

    ;; Set overlay + temporary buffer.
    (let* ((ov (make-overlay beg end nil t))
           (proc-buf (gptel--temp-buffer " *gptel-rewrite*"))
           (info (list :context (cons ov proc-buf))))
      (overlay-put ov 'category 'gptel)
      (overlay-put ov 'evaporate t)

      ;; Send a request, and display the result via gptel--rewrite-callback.
      (gptel-request context
        :system gptel-watch-system-prompt
        :callback (lambda (response _reqinfo)
                    (gptel--rewrite-callback response info))))))

(defun gptel-watch--maybe-request ()
  "Check if current line matches trigger and call GPT if so."
  (when (gptel-watch--line-matches-p)
    (gptel-watch--handle-request)))

(defun gptel-watch--post-command-hook ()
  "Run after a command, check if it should trigger GPT generation."
  (when (and (not (minibufferp))
             (memq this-command gptel-watch-trigger-commands))
    (save-excursion
      (forward-line -1)
      (gptel-watch--maybe-request))))

;;;###autoload
(define-minor-mode gptel-watch-mode
  "Automatically call `gptel-request` when typing lines ending with a trigger pattern.
This mode listens to commands like `newline` or `org-return`, and if the
previous line ends with a string in `gptel-watch-trigger-patterns`,
the line is cleared and surrounding context is sent to GPT for continuation."
  :lighter " WatchAI"
  :group 'gptel-watch
  (if gptel-watch-mode
      (add-hook 'post-command-hook #'gptel-watch--post-command-hook nil t)
    (remove-hook 'post-command-hook #'gptel-watch--post-command-hook t)))

(defun gptel-watch--enable-if-eligible ()
  "Enable `gptel-watch-mode` if not in minibuffer."
  (unless (minibufferp)
    (gptel-watch-mode 1)))

;;;###autoload
(define-globalized-minor-mode gptel-watch-global-mode
  gptel-watch-mode
  gptel-watch--enable-if-eligible
  :group 'gptel-watch
  :init-value nil
  :lighter " WatchAI"
  "Globalized version of `gptel-watch-mode`.")

(provide 'gptel-watch)

;;; gptel-watch.el ends here
