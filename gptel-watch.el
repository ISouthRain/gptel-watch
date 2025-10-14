;;; gptel-watch.el --- Auto call gptel-request based on trigger patterns -*- lexical-binding: t; -*-

;; Author: ISouthRain
;; Version: 0.2.2
;; Package-Requires: ((emacs "27.1") (gptel "0.9.8.5"))
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

(defvar gptel-watch--current-context nil
  "Current context.")

(defvar gptel-watch--line-history nil
  "History list for User input line format.")

;;;###autoload
(defun gptel-watch ()
  "Manually invoke GPT context generation on current line if it matches any trigger."
  (interactive)
  (if (gptel-watch--line-matches-p)
      (gptel-watch--request)
    (user-error "[gptel-watch] No AI line.")))

(defun gptel-watch--log (fmt &rest args)
  "Internal logging utility for gptel-watch."
  (apply #'message (concat "[gptel-watch] " fmt) args))

(defun gptel-watch--line-matches-p ()
  "Return non-nil if the current line ends with a trigger pattern."
  (let ((line (thing-at-point 'line t)))
    (when line
      (cl-some (lambda (pat) (string-match-p (concat pat "$") line))
               gptel-watch-trigger-patterns))))

(defun gptel-watch--extract-context-defun ()
  "Extract text of the current defun, including one line before it."
  (save-excursion
    (mark-defun)
    (prog1
        (buffer-substring-no-properties (region-beginning) (region-end))
      (deactivate-mark))))

(defun gptel-watch--extract-context-page ()
  "Extract text of the current page (`mark-page`)."
  (save-excursion
    (mark-page)
    (prog1
        (buffer-substring-no-properties (region-beginning) (region-end))
      (deactivate-mark))))

(defun gptel-watch--extract-context-lines-relative ()
  "Extract context based on relative lines above and below point."
  (condition-case err
      (let* ((input (read-string "Enter UP,DOWN lines (e.g. 10,20): " nil 'gptel-watch--line-history)))
        (unless (string-match-p "^[0-9]+,[0-9]+$" input)
          (user-error "Invalid input format. Expected e.g. 10,20"))
        (let* ((parts (split-string input ","))
               (up (string-to-number (car parts)))
               (down (string-to-number (cadr parts)))
               (start (save-excursion
                        (forward-line (- up))
                        (line-beginning-position)))
               (end (save-excursion
                      (forward-line down)
                      (line-end-position))))
          (buffer-substring-no-properties start end)))
    ((quit user-error)
     (message "[gptel-watch] Cancelled or invalid input (Down/Up Line).")
     nil)))

(defun gptel-watch--extract-context-lines-range ()
  "Extract context between two absolute line numbers."
  (condition-case err
      (let* ((input (read-string "Enter START,END line numbers (e.g. 100,200): " nil 'gptel-watch--line-history)))
        (unless (string-match-p "^[0-9]+,[0-9]+$" input)
          (user-error "Invalid input format. Expected e.g. 100,200"))
        (let* ((parts (split-string input ","))
               (start-line (string-to-number (car parts)))
               (end-line (string-to-number (cadr parts))))
          (when (<= end-line start-line)
            (user-error "END line must be greater than START line"))
          (let ((start (save-excursion
                         (goto-char (point-min))
                         (forward-line (1- start-line))
                         (point)))
                (end (save-excursion
                       (goto-char (point-min))
                       (forward-line (1- end-line))
                       (line-end-position))))
            (buffer-substring-no-properties start end))))
    ((quit user-error)
     (message "[gptel-watch] Cancelled or invalid input (Line Range).")
     nil)))

(defun gptel-watch--extract-context-current-line ()
  "Extract text of the current line only."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun gptel-watch--extract-context ()
  "Extract context interactively using one of several methods:
1. Defun(mark-defun)
2. Page(mark-page)
3. Down/Up Line
4. Line Range
5. Only Current Line
Return NIL if user cancels or input invalid."
  (condition-case nil
      (let* ((choice (completing-read
                      "Choose context method: "
                      '("Defun(mark-defun)"
                        "Page(mark-page)"
                        "Down/Up Line"
                        "Line Range"
                        "Only Current Line")
                      nil t)))
        (pcase choice
          ("Defun(mark-defun)" (gptel-watch--extract-context-defun))
          ("Page(mark-page)" (gptel-watch--extract-context-page))
          ("Down/Up Line" (gptel-watch--extract-context-lines-relative))
          ("Line Range" (gptel-watch--extract-context-lines-range))
          ("Only Current Line" (gptel-watch--extract-context-current-line))
          (_ nil)))
    (quit
     (message "[gptel-watch] Cancelled context extraction.")
     nil)))

(defun gptel-watch--request ()
  "Send extracted context to GPT and show diff overlay with the result."
  (let ((context (gptel-watch--extract-context)))
    (unless context
      (gptel-watch--log "User proactively stopped."))
    (when context   ;; Add a check here, exit directly if nil.
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (setq gptel-watch--current-context context)
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
                        (gptel--rewrite-callback response info))))))))

(defun gptel-watch--post-command ()
  "Run after a command, check user AI intertion."
  (when (and (not (minibufferp))
             (memq this-command gptel-watch-trigger-commands))
    (save-excursion
      (forward-line -1) ;; Because new line, So.
      (when (gptel-watch--line-matches-p)
        (forward-line 1) ;; go to the new line.
        (delete-line) ;; Remove the new line.
        (forward-line -1) ;; go to the AI line.
        (gptel-watch--request)))))

;;;###autoload
(defun gptel-watch-switch-prompt ()
  "Set `gptel-watch-system-prompt' from `gptel-directives'."
  (interactive)
  (condition-case nil
      (let* ((choice (completing-read
                      "Choose system prompt: "
                      (mapcar (lambda (x) (symbol-name (car x))) gptel-directives)
                      nil t)) ; t indicates that an option must be matched.
             (content (cdr (assoc (intern choice) gptel-directives))))
        (setq gptel-watch-system-prompt content))))

;;;###autoload
(define-minor-mode gptel-watch-mode
  "Watch User's AI intention."
  :lighter " WatchAI"
  :group 'gptel-watch
  (if gptel-watch-mode
      (add-hook 'post-command-hook #'gptel-watch--post-command nil t)
    (remove-hook 'post-command-hook #'gptel-watch--post-command t)))

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
