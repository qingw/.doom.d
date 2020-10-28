;;; zcheng/misc/autoload.el -*- lexical-binding: t; -*-

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
