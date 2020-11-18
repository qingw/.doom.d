;;; +funcs.el -*- lexical-binding: t; -*-


;; Current time and date
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%H:%M"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time)))
  )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time)))
  )


;;;###autoload
(defun my/web-html-setup()
  "Setup for web-mode html files."
  (message "web-mode use html related setup")
  ;; (flycheck-add-mode 'html-tidy 'web-mode)
  ;; (flycheck-select-checker 'html-tidy)
  (add-to-list (make-local-variable 'company-backends)
               '(company-web-html company-files company-css company-capf company-dabbrev))
  (add-hook 'before-save-hook #'sgml-pretty-print))

;;;###autoload
(defun my/web-vue-setup()
  "Setup for js related."
  (message "web-mode use vue related setup")
  (setup-tide-mode)
  (prettier-js-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-select-checker 'javascript-eslint)
  (my/use-eslint-from-node-modules)
  (add-to-list (make-local-variable 'company-backends)
               '(comany-tide company-web-html company-css company-files))
  )

;;;###autoload
(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

;;;###autoload
(defun gcl/git-push ()
  "Execute the command."
  (interactive)
	(progn
		(gcl/exec-command (concat "~/.gclrc/shl/git-push.sh " (file-name-directory buffer-file-name)))
    (ace-select-window)
		;; second, repeat, func, message
		(run-at-time "1 sec" nil 'kill-current-buffer)))

;;;###autoload
(defun gcl/exec-command (cmd)
  "Execute the command: `CMD` with description ARGS."
  (interactive)
  (let* ((command (or (and (boundp 'executable-command) executable-command) cmd))
         (compilation-ask-about-save nil))
    (executable-interpret (read-shell-command "Run: " command))))

;;;###autoload
(defun gcl/git-push-delete-window ()
	(progn
		(message "Git push should done or running in background.")
		(delete-window)))

;;;###autoload
(defun gcl/goto-match-paren (arg)
  "Go to the matching if on (){}[], similar to vi style of % ."
  (interactive "p")
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

;;;###autoload
(defun gcl/string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   (t
    ;; default
    (string-inflection-java-style-cycle))))
