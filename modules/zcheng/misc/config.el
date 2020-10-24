;;; zcheng/misc/config.el -*- lexical-binding: t; -*-

;; (defconst my-leader "SPC")

;; global mode enable
(setq-default abbrev-mode t)
(global-prettify-symbols-mode t)
;; --- end

;;;; keybindings
(map! :n "C-=" #'er/expand-region
      :n "C-+" #'text-scale-increase
      :n "C-(" #'sp-backward-slurp-sexp
      :n "C-)" #'sp-forward-slurp-sexp
      :n "C-{" #'sp-backward-sexp
      :n "C-}" #'sp-forward-sexp
      :n "s-(" #'sp-forward-barf-sexp
      :n "s-)" #'sp-backward-barf-sexp
      :n "s-<" #'move-text-up
      :n "s->" #'move-text-down)

(map! :leader
      (:prefix ("v" . "View")
       :n "p" #'ivy-push-view
       :n "o" #'ivy-pop-view
       :n "." #'ivy-switch-view)
      ;; g
      :n "gP" #'gcl/git-push
      :n "lm" #'lsp-ui-imenu
      )

;;;; -end keybindings

(use-package! evil
  :init
  (progn
    (define-key! evil-normal-state-map
      "g[" #'beginning-of-defun
      "g]" #'end-of-defun
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

