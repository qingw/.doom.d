;;; zcheng/misc/config.el -*- lexical-binding: t; -*-

;; (defconst my-leader "SPC")

;; global mode enable
(setq-default abbrev-mode t)
(setq-default fill-column 80)
(fset 'yes-or-no-p 'y-or-n-p)
(global-prettify-symbols-mode t)
(global-pangu-spacing-mode 1)
(global-eldoc-mode -1)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;; enble the xref backend
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setenv "PATH" (concat (getenv "PATH") ":/Users/simon/go"))
;; (add-hook 'kill-emacs-hook #'+workspace/kill-session)
;; --- end

;;;; -end keybindings
(use-package! evil
  :init
  (progn
    (define-key! evil-normal-state-map
      "g[" #'beginning-of-defun
      "g]" #'end-of-defun
      "gd" #'xref-find-definitions
      "gD" #'xref-find-references
      "gb" #'xref-pop-marker-stack
      "gjj" #'dumb-jump-go
      "gjb" #'dumb-jump-back
      "z-" #'sp-splice-sexp
      "z." #'emmet-wrap-with-markup
      "+" #'evil-numbers/inc-at-pt
      "-" #'evil-numbers/dec-at-pt)
    (define-key! evil-visual-state-map
      "z." #'emmet-wrap-with-markup)))

(use-package! visual-regexp
  :commands (vr/select-replace vr/select-query-replace))

(use-package! visual-regexp-steriods
  :commands (vr/select-replace vr/select-query-replace)
  :init
  (progn
    (define-key global-map (kbd "C-c r") 'vr/replace)
    (define-key global-map (kbd "C-c q")' vr/query-replace)))

(use-package! smart-hungry-delete
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :defer nil
  :config (smart-hungry-delete-add-default-hooks))

(use-package! parrot
  :config
  (parrot-mode))

(use-package! delsel
  :hook (after-init . delete-selection-mode))
