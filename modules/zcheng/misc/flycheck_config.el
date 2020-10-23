;;; zcheng/misc/config.el -*- lexical-binding: t; -*-

;; global mode enable
(setq-default abbrev-mode t)
;; --- end

;;;; keybindings
(evil-define-key* 'normal 'global
  (kbd "C-=") #'er/expand-region
  (kbd "C-+") #'text-scale-increase
  (kbd "C-(") #'sp-backward-slurp-sexp
  (kbd "C-)") #'sp-forward-slurp-sexp
  (kbd "C-{") #'sp-backward-sexp
  (kbd "C-}") #'sp-forward-sexp
  (kbd "s-(") #'sp-forward-barf-sexp
  (kbd "s-)") #'sp-backward-barf-sexp

  ;; SPC x x
  (kbd "SPC g P") #'gcl/git-push
  (kbd "SPC v p") #'ivy-push-view
  (kbd "SPC v o") #'ivy-pop-view
  (kbd "SPC v .") #'ivy-switch-view
  (kbd "SPC i c") #'counsel-colors-web
  (kbd "SPC i C") #'counsel-colors-emacs
  )
(map! :n "C-=" #'er/expand-region
      :n "C-+" #'text-scale-increase
      :n "C-(" #'sp-backward-slurp-sexp
      :n "C-)" #'sp-forward-slurp-sexp
      :n "C-{" #'sp-backward-sexp
      :n "C-}" #'sp-forward-sexp
      :n "s-(" #'sp-forward-barf-sexp
      :n "s-)" #'sp-backward-barf-sexp)

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
