;;; zcheng/misc/config.el -*- lexical-binding: t; -*-

;; (defconst my-leader "SPC")

;; global mode enable
(setq-default abbrev-mode t)
(global-prettify-symbols-mode t)
(global-pangu-spacing-mode 1)
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
      :n "s->" #'move-text-down
      ;; multiple cursors
      "C->" #'mc/mark-next-like-this
      "C-<" #'mc/mark-previous-like-this
      "C-c C-<" #'mc/mark-all-like-this
      "C-S-c C-S-c" #'mc/edit-lines
      "C-S-c 0" #'mc/insert-numbers
      "C-S-c 1" #'mc/insert-letters
      "C-S-c s" #'mc/mark-all-in-region
      "C-S-c S" #'mc/mark-all-in-region-regexp

      ;; crux
      "C-c o" #'crux-open-with
      "C-c u" #'crux-view-url
      "C-c D" #'crux-delete-buffer-and-file
      "C-c S" #'crux-find-shell-init-file
      ;; hydra
      "C-c h" nil
      "C-c h h" #'hydra-main/body
      "C-c h t" #'hydra-tip/body
      )

(map! :leader
      ;; a
      :n "ar" #'ranger
      (:prefix ("v" . "View")
       :n "p" #'ivy-push-view
       :n "o" #'ivy-pop-view
       :n "." #'ivy-switch-view)
      ;; f
      :n "fo" #'crux-open-with
      ;; g
      :n "gP" #'gcl/git-push
      ;; l
      :n "lm" #'lsp-ui-imenu
      :n "lt" #'treemacs
      :n "ll" #'+workspace/switch-to
      ;; x
      :n "x" nil
      (:prefix ("x" . "Trash")
       :n "x" #'doom/open-scratch-buffer)
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


(use-package! smart-hungry-delete
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :defer nil
  :config (smart-hungry-delete-add-default-hooks))
