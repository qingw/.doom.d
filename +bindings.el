;;; +bindings.el -*- lexical-binding: t; -*-

;; unbind
(map! :leader
      "A" nil
      "X" nil)

(map! "C-s" nil
      "C-c h" nil
      "s-q" nil)

;; unbind functions
(map! [remap swiper] #'swiper-isearch)
(map! [remap org-capture] nil)

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
       :desc "Dired ranger"             "r" #'ranger
       ) ;; a - end

      (:prefix ("b" . "buffer")
       :desc "Open in finder"           "f" #'osx-lib-reveal-in-finder
       ) ;; b - end

      (:prefix ("e" . "errors")
       :desc "Flycheck list errors"        "l" #'flycheck-list-errors
       :desc "Disable flycheck"            "d" #'flycheck-disable-checker
       :desc "Enable flycheck"             "C" #'flycheck-buffer
       :desc "Flycheck next error"         "n" #'flycheck-next-error
       :desc "Flycheck previous error"     "p" #'flycheck-previous-error
       :desc "Flycheck clear errors"       "c" #'flycheck-clear
       :desc "Flycheck which checker"      "w" #'flycheck-select-checker
       ) ;; e - end

      (:prefix ("f" . "file")
       :desc "Dired jump"                  "j" #'dired-jump
       :desc "Open with specify app"       "o" #'crux-open-with
       ) ;; f - end

      (:prefix ("j" . "jump")
       :desc "Jump to symbol"              "i" #'imenu
       ) ;; j - end

      (:prefix ("v" . "view")
       :desc "Push view"                "p" #'ivy-push-view
       :desc "Pop view"                 "o" #'ivy-pop-view
       :desc "Switch view"              "." #'ivy-switch-view
       ) ;; v - end

      (:prefix ("w" . "+window")
       :desc "Split window below"          "-" #'split-window-below
       ) ;; w - end

      )  ;; map - end

;; 指定模式下按键映射
;; web-mode
(map! (:map web-mode-map
       :localleader
       :desc "Instant rename tag"       "r" #'instant-rename-tag
       ))

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
      "C-c U" #'browse-url-at-point
      "C-c D" #'crux-delete-buffer-and-file
      "C-c S" #'crux-find-shell-init-file
      ;; hydra
      "C-c h h" #'hydra-main/body
      "C-c h t" #'hydra-tip/body
      "C-c h l" #'hydra-launcher/body
      ;; s, Command
      "s-q" #'+workspace/kill-session-and-quit

      ;; M-option/alt key
      "M--" #'gcl/goto-match-paren
      "M-i" #'gcl/string-inflection-cycle-auto
      )

