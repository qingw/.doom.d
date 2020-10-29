;;; zcheng/misc/config.el -*- lexical-binding: t; -*-

;; (defconst my-leader "SPC")

;; global mode enable
(setq-default abbrev-mode t)
(global-prettify-symbols-mode t)
(global-pangu-spacing-mode 1)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 80)
;; enble the xref backend
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;; --- end

(map! "C-s" nil
      "C-c h" nil)
;;;; keybindings
(map! :n "C-=" #'er/expand-region
      :n "C-+" #'cnfonts-increase-fontsize
      :n "C--" #'cnfonts-decrease-fontsize
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
      ;; C-s(control - command)
      "C-s-," #'parrot-rotate-prev-word-at-point
      "C-s-." #'parrot-rotate-next-word-at-point
      ;; crux
      "C-c o" #'crux-open-with
      "C-c u" #'crux-view-url
      "C-c D" #'crux-delete-buffer-and-file
      "C-c S" #'crux-find-shell-init-file
      ;; hydra
      "C-c h h" #'hydra-main/body
      "C-c h t" #'hydra-tip/body

      ;; M-option/alt key
      "M--" #'gcl/goto-match-paren
      "M-i" #'gcl/string-inflection-cycle-auto
      )

(map! :leader
      ;; a
      :n "ar" #'ranger
      ;; b
      :n "bf" #'osx-lib-reveal-in-finder
      ;; f
      :n "fo" #'crux-open-with
      ;; g
      :n "gP" #'gcl/git-push
      ;; l
      :n "lm" #'lsp-ui-imenu
      :n "lt" #'treemacs
      :n "ll" #'+workspace/switch-to
      ;; w
      :n "w-" #'evil-window-split
      ;; x
      :n "x" nil
      (:prefix ("x" . "Trash")
       :n "x" #'doom/open-scratch-buffer)
      (:prefix ("v" . "View")
       :n "p" #'ivy-push-view
       :n "o" #'ivy-pop-view
       :n "." #'ivy-switch-view)
      )

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

