;;; +bindings.el -*- lexical-binding: t; -*-


(global-set-key (kbd "C-c d") 'insert-current-date-time)
(global-set-key (kbd "C-c t") 'insert-current-time)

;; unbind
(map! :leader
      "A" nil
      "X" nil)

(map! "C-s" nil
      "C-c h" nil
      "s-q" nil
      "M-," nil
      "M-." nil)

;; unbind functions
(map! [remap swiper] #'swiper-isearch
      [remap org-capture] nil
      [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
      [remap xref-find-references] #'lsp-ui-peek-find-references)

;; leader
(map! :leader
      :desc "M-x"                       "SPC" #'execute-extended-command
      ;; g
      :desc "Git auto push"             "gP"  #'gcl/git-push
      ;; l
      :desc "Show lsp ui imenu"         "lm"  #'lsp-ui-imenu
      :desc "Switch workspace"          "ll"  #'+workspace/switch-to
      )

;; Prefix
(map! :leader
      (:prefix ("a" . "Apps")
       "r" #'ranger
       ) ;; a - end

      "bf" #'osx-lib-reveal-in-finder

      (:prefix ("e" . "errors")
       "l" #'flycheck-list-errors
       "d" #'flycheck-disable-checker
       "C" #'flycheck-buffer
       "n" #'flycheck-next-error
       "p" #'flycheck-previous-error
       "c" #'flycheck-clear
       "w" #'flycheck-select-checker
       ) ;; e - end

      "fj" #'dired-jump
      "fo" #'crux-open-with

      (:prefix ("j" . "jump")
       "i" #'imenu
       ) ;; j - end

      ;; s
      "sj" #'lsp-ivy-workspace-symbol
      "sJ" #'+ivy/jump-list
      ;; :desc "Jump to symbol in current ws" "sj" #'lsp-
      (:prefix ("v" . "view")
       "p" #'ivy-push-view
       "o" #'ivy-pop-view
       "." #'ivy-switch-view
       ) ;; v - end

      "w-" #'split-window-below
      )  ;; map - end

;; 指定模式下按键映射
;; web-mode
(map! (:map web-mode-map
       :desc "Hydra body"       :n      "." #'hydra-web-mode/body
       :localleader
       :desc "Instant rename tag"       "r" #'instant-rename-tag
       ))

;; evil
(map!
 :desc "Go function header"     :n "g[" #'beginning-of-defun
 :desc "Go function end"        :n "g]" #'end-of-defun
 :desc "Find definition"        :n "gd" #'xref-find-definitions
 :desc "Find reference"         :n "gD" #'xref-find-references
 :desc "Go back find piont"     :n "gb" #'xref-pop-marker-stack
 :desc "Delete parens"          :n "z-" #'sp-splice-sexp
 :desc "Wrap with markup"       :nv "z." #'emmet-wrap-with-markup
 :desc "Increase number"        :n "+"  #'evil-numbers/inc-at-pt
 :desc "Decrease number"        :n "-"  #'evil-numbers/dec-at-pt)
;;;; keybindings
(map! :n "C-=" #'er/expand-region
      :n "C-+" #'cnfonts-increase-fontsize
      :n "C--" #'cnfonts-decrease-fontsize
      :n "C-(" #'sp-backward-slurp-sexp
      :n "C-)" #'sp-forward-slurp-sexp
      :n "C-{" #'sp-backward-sexp
      :n "C-}" #'sp-forward-sexp
      :nv "C-e" #'evil-end-of-line
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
      "C-c r" #'vr/replace
      "C-c q" #'vr/query-replace
      ;; crux
      "C-c o" #'crux-open-with
      "C-c u" #'crux-view-url
      "C-c U" #'browse-url-at-point
      "C-c D" #'crux-delete-buffer-and-file
      "C-c S" #'crux-find-shell-init-file
      ;; hydra
      "C-c h h" #'hydra-main/body
      "C-c h t" #'hydra-tip/body
      "C-c h l" #'hydra-launcher/body
      "C-c h w" #'hydra-web-mode/body
      ;; s, Command
      "s-q" #'+workspace/kill-session-and-quit

      ;; M-option/alt key
      "M--" #'gcl/goto-match-paren
      "M-i" #'gcl/string-inflection-cycle-auto
      )
