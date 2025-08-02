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

(defcustom gptel-watch-trigger-regexp "AI!"
  "Regexp to detect trigger line for `gptel-watch`."
  :type 'regexp
  :group 'gptel-watch)

(defvar gptel-watch-prompt
  "你作为一个编程编码助手, 你了解所有编程语言.
当我当我发送代码给你时, 你推测我的用途, 帮我编写代码.
比如我发送:
int main()
{
  // 打印 Hello World. AI!
}
如果有行文本字符: AI! 那么你就推测文本 AI! 行用意, 然后你返回你写代码.
仅仅返回你写的代码内容, 比如:
printf(\"Hello World\");

下面是限制你返回的条件:
请不要发送任何 Markdown 格式代码:
```language
Code
```
请不要发送任何 Markdown 格式代码.
请不要发送任何 Markdown 格式代码.
简洁回复.
")

(defun gptel-watch--debug-log (format-str &rest args)
  "Print debug log to *Messages* buffer."
  (apply #'message (concat "[gptel-watch] " format-str) args))

(defun gptel-watch--check-newline-and-trigger ()
  "Check if just inserted newline and previous line contains trigger text."
  (when (and (eq last-command-event ?\n)
             (not (minibufferp)))
    (save-excursion
      (forward-line -1)
      (let ((line (thing-at-point 'line t)))
        (when (and line
                   (string-match-p (concat gptel-watch-trigger-regexp "$") line))
          (gptel-watch))))))

;;;###autoload
(defun gptel-watch ()
  "If current line matches trigger, extract context and call `gptel-request`."
  (interactive)
  (gptel-watch--debug-log "Calling `gptel-watch`.")
  (let ((current-line (thing-at-point 'line t)))
    (if (and current-line
             (string-match-p (concat gptel-watch-trigger-regexp "$") current-line))
        (let* ((start (save-excursion (forward-line -10) (line-beginning-position)))
               (end   (save-excursion (forward-line 10)  (line-end-position)))
               (context (buffer-substring-no-properties start end))
               (line-start (line-beginning-position))
               (line-end   (line-end-position)))
          ;; 清除当前行
          (delete-region line-start line-end)
          ;; 调用 GPT 请求
          (gptel-request context :system gptel-watch-prompt)))))

(defun gptel-watch--maybe-enable ()
  "Enable `gptel-watch-mode` if not in minibuffer."
  (unless (minibufferp)
    (gptel-watch-mode 1)))

;;;###autoload
(define-minor-mode gptel-watch-mode
  "Minor mode to auto-call `gptel-request` when typing matching lines."
  :lighter " WatchAI"
  :group 'gptel-watch
  (if gptel-watch-mode
      (progn
        (add-hook 'post-self-insert-hook #'gptel-watch--check-newline-and-trigger nil t))
    (remove-hook 'post-self-insert-hook #'gptel-watch--check-newline-and-trigger t)))

;;;###autoload
(define-globalized-minor-mode gptel-watch-global-mode
  gptel-watch-mode
  gptel-watch--maybe-enable
  :group 'gptel-watch
  :init-value nil
  )

(provide 'gptel-watch)

;;; gptel-watch.el ends here
