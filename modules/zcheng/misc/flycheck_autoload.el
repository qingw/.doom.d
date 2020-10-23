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
