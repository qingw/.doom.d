;;; zcheng/dev/config.el -*- lexical-binding: t; -*-

(use-package! lsp-mode
  ;; :hook (go-mode . lsp)
  ;; :commands lsp
  :init
  (setq lsp-gopls-server-args '("-debug" ":8080" "-vv" "-logfile" "/Users/simon/gopls.log"))
  (defun lsp-go-install-save-hooks ()
    "Comment..."
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection "gopls")
;;                   :major-modes '(go-mode)
;;                   :priority 0
;;                   :initialization-options 'lsp-clients-go--make-init-options
;;                   :server-id 'gopls
;;                   :library-folders-fn (lambda (_workspace)
;;                                         lsp-clients-go-library-directories)))
;; (setq lsp-clients-go-command "/Users/simon/go/bin/gopls")
