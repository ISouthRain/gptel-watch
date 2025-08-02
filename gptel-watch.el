;;; gptel-watch.el --- Auto trigger gptel-request on "AI!" lines -*- lexical-binding: t; -*-

;; Author: ISouthRain
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))
;; Keywords: AI, convenience
;; URL: https://github.com/ISouthRain/gptel-watch

;;; Commentary:

;; This minor mode automatically watches for lines ending with "AI!"
;; and when the user presses RET after such a line, it extracts context,
;; clears the line, and calls `gptel-request` using the `WatchAI` directive.

;;; Code:

(require 'gptel)

(defgroup gptel-watch nil
  "Automatically trigger gptel requests when typing lines ending in AI!"
  :group 'convenience
  :prefix "gptel-watch-")

(defcustom gptel-watch-trigger-patterns '("AI" "AI!" "#ai")
  "List of regexp patterns to trigger `gptel-watch` when they appear at end of line."
  :type '(repeat regexp)
  :group 'gptel-watch)

(defcustom gptel-watch-commands '(newline
                                  org-return)
  "List of commands that should trigger `gptel-watch--check-newline-and-trigger`."
  :type '(repeat (function :tag "Command")))

(defvar gptel-watch-prompt
  "你作为一个文本助手, 你拥有写作和编程能力.
你根据上下文, 推测我的意图, 帮我编写内容.
比如我发送:
int main()
{
  // 打印 Hello World. AI!
}
那么你就推测 文本 AI! 行用意, 然后你返回内容.
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
")

(defun gptel-watch--debug-log (format-str &rest args)
  "Print debug log to *Messages* buffer."
  (apply #'message (concat "[gptel-watch] " format-str) args))

(defun gptel-watch--should-trigger-p (line)
  "Return non-nil if LINE ends with any of `gptel-watch-trigger-patterns`."
  (when line
    (cl-some (lambda (pat)
               (string-match-p (concat pat "$") line))
             gptel-watch-trigger-patterns)))

(defsubst gptel-watch--trigger-command-p ()
  "Check if `this-command' is one of `gptel-watch-commands'."
  (memq this-command gptel-watch-commands))

(defun gptel-watch--process-current-line ()
  "If current line matches trigger, extract context and call `gptel-request`."
  (let ((current-line (thing-at-point 'line t)))
    (when (gptel-watch--should-trigger-p current-line)
      (let* ((start (save-excursion (forward-line -10) (line-beginning-position)))
             (end   (save-excursion (forward-line 10)  (line-end-position)))
             (context (buffer-substring-no-properties start end))
             (line-start (line-beginning-position))
             (line-end   (line-end-position)))
        ;; Clear the current line content (keep newline character)
        (delete-region line-start line-end)
        ;; Calling GPT request
        (gptel-watch--debug-log "Respond in buffer...")
        (gptel-request context :system gptel-watch-prompt)))))

(defun gptel-watch--check-newline-and-trigger ()
  "If the previous line matches trigger patterns, process it."
  (unless (minibufferp)
    (save-excursion
      (forward-line -1)
      (gptel-watch--process-current-line))))

;;;###autoload
(defun gptel-watch ()
  "Manually trigger GPT request if current line matches pattern."
  (interactive)
  (gptel-watch--process-current-line))

(defun gptel-watch--maybe-enable ()
  "Enable `gptel-watch-mode` if not in minibuffer."
  (unless (minibufferp)
    (gptel-watch-mode 1)))

(defun gptel-watch--post-command-hook ()
  "Run after commands; trigger GPT check if `this-command' is in `gptel-watch-commands`."
  (when (and (not (minibufferp))
             (gptel-watch--trigger-command-p))
    (gptel-watch--check-newline-and-trigger)))

;;;###autoload
(define-minor-mode gptel-watch-mode
  "Minor mode to auto-call `gptel-request` after certain commands like `newline`.

When enabled, this mode monitors user commands using `post-command-hook`.
If the command is in `gptel-watch-commands` (e.g., `newline`, `org-return`),
and the previous line matches a trigger in `gptel-watch-trigger-patterns`,
the line is cleared and GPT is called with surrounding context."
  :lighter " WatchAI"
  :group 'gptel-watch
  (if gptel-watch-mode
      (add-hook 'post-command-hook #'gptel-watch--post-command-hook nil t)
    (remove-hook 'post-command-hook #'gptel-watch--post-command-hook t)))

;;;###autoload
(define-globalized-minor-mode gptel-watch-global-mode
  gptel-watch-mode
  gptel-watch--maybe-enable
  :group 'gptel-watch
  :init-value nil
  "Globalized version of `gptel-watch-mode`")

(provide 'gptel-watch)

;;; gptel-watch.el ends here
