;;; zcheng/prog/config.el -*- lexical-binding: t; -*-

;; (use-package! lsp-mode)

;; (use-package! company
;;   :demand t
;;   :commands company-mode
;;   :config
;;   ;; Enable company-mode globally.
;;   (global-company-mode)
;;   ;; Except when you're in term-mode.
;;   (setq company-global-modes '(not term-mode))
;;   ;; Give Company a decent default configuration.
;;   (setq company-minimum-prefix-length 2
;;         company-selection-wrap-around t
;;         company-show-numbers t
;;         company-tooltip-align-annotations t
;;         company-require-match nil
;;         company-dabbrev-downcase nil
;;         company-dabbrev-ignore-case nil)
;;   ;; Sort completion candidates that already occur in the current
;;   ;; buffer at the top of the candidate list.
;;   (setq company-transformers '(company-sort-by-occurrence))
;;   ;; Show documentation where available for selected completion
;;   ;; after a short delay.
;;   (use-package company-quickhelp
;;     :config
;;     (setq company-quickhelp-delay 1)
;;     (company-quickhelp-mode 1))
;;   ;; Add a completion source for emoji. 😸
;;   (use-package company-emoji
;;     :config
;;     (company-emoji-init))
;;   ;; Use C-\ to activate the Company autocompleter.
;;   ;; We invoke company-try-hard to gather completion candidates from multiple
;;   ;; sources if the active source isn't being very forthcoming.
;;   (use-package company-try-hard
;;     :commands company-try-hard
;;     :bind ("C-\\" . company-try-hard)
;;     :config
;;     (bind-keys :map company-active-map
;;                ("C-\\" . company-try-hard)))
;;   :diminish company-mode)
