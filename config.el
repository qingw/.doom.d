;;;###autoload
(defun gcl/goto-match-paren (arg)
  "Go to the matching if on (){}[], similar to vi style of % ."
  (interactive "p")
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

;;;###autoload
(defun gcl/string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   (t
    ;; default
    (string-inflection-all-cycle))))

;; Current time and date
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%H:%M"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time)))
  )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time)))
  )

(defun my/capitalize-first-char (&optional string)
  "Capitalize only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))
(defun my/lowcase-first-char (&optional string)
  "Capitalize only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat first-char rest-str))))

;;; config.el -*- lexical-binding: t; -*-

;; 解绑一些按键，待用
(map! :niv      "C-s" nil
      :niv      "C-d" nil
      :niv      "C-i" nil
      :niv      "M-," nil
      :niv      "M-." nil
      :niv      "M-f" nil

      :leader
      "A" nil
      "X" nil
      "/" nil)

(map! [remap swiper] #'swiper-isearch
      [remap org-capture] nil
      [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
      [remap xref-find-references] #'lsp-ui-peek-find-references
      )

(global-set-key (kbd "<f3>") 'hydra-multiple-cursors/body)
(global-set-key (kbd "<f5>") 'deadgrep)
(global-set-key (kbd "<M-f5>") 'deadgrep-kill-all-buffers)
(global-set-key (kbd "<f12>") 'smerge-vc-next-conflict)
(global-set-key (kbd "<f11>") '+vc/smerge-hydra/body)

(map! :leader
      :n        "SPC"   #'execute-extended-command
      :n        "bf"   #'osx-lib-reveal-in-finder
      :n        "fo"   #'crux-open-with
      :n        "fj"   #'dired-jump
      :n        "/r"   #'deadgrep

      (:prefix ("l" . "load")
       :n       "i"     #'imenu-list
       :n       "o"     #'lsp-ui-imenu
       :n       "d"     #'deft
       :n       "l"     #'+workspace/switch-to)

      (:prefix ( "v" . "view" )
       :n       "o"     #'ivy-pop-view
       :n       "p"     #'ivy-push-view)

      :n        "w -"   #'split-window-below
      )

(map! "s-<"     #'move-text-up
      "s->"     #'move-text-down
      "s-i"     #'gcl/string-inflection-cycle-auto
      "s-("     #'sp-backward-barf-sexp
      "s-)"     #'sp-forward-barf-sexp

      )

(map!
 "C-'"     #'imenu-list-smart-toggle
 "C-d"     (cmd! (previous-line)
                 (kill-line)
                 (forward-line))
 "C-s"     #'+default/search-buffer

 ;; smartparen
 "C-("     #'sp-backward-slurp-sexp
 "C-)"     #'sp-forward-slurp-sexp


 ;; multiple cursors
 "C->"     #'mc/mark-next-like-this
 "C-<"     #'mc/mark-previous-like-this
 "C-c C-<" #'mc/mark-all-like-this
 "C-S-c C-S-c" #'mc/edit-lines
 "C-S-c 0" #'mc/insert-numbers
 "C-S-c 1" #'mc/insert-letters
 "C-S-c s" #'mc/mark-all-in-region
 "C-S-c S" #'mc/mark-all-in-region-regexp

 ;; prefix C-c
 "C-c a c"     #'org-mac-chrome-insert-frontmost-url
 "C-c d"       #'insert-current-date-time
 "C-c t"       #'insert-current-time
 "C-c o"       #'crux-open-with
 "C-c r"       #'vr/replace
 "C-c q"       #'vr/query-replace
 "C-c u"       #'crux-view-url
 "C-c y"       #'youdao-dictionary-search-at-point+

 "C-c C-f"     #'json-mode-beautify

 :niv      "C-e"     #'evil-end-of-line
 :niv      "C-="     #'er/expand-region

 )

(map! "M--"     #'gcl/goto-match-paren
      "M-i"     #'parrot-rotate-next-word-at-point
      "M-f"     #'scroll-up-command)

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

(map! :map web-mode-map
      "<f2>"    #'hydra-web-mode/body

      :map org-mode-map
      :n       "tt" #'org-todo
      :n       "tc" #'org-toggle-checkbox
      :n       "tpp" #'org-priority
      :n       "tpu" #'org-priority-up
      :n       "tpd" #'org-priority-down
      )

;; 个人信息配置
(setq user-full-name "Zhicheng Lee"
      user-mail-address "gccll.love@gmail.com"
      user-blog-url "https://www.cheng92.com")

;; setq, set-default 统一配置的地方
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq org-directory "~/github/documents/org")
(setq display-line-numbers-type t)

(setq-default
 fill-column 80
 undo-limit 80000000
 delete-by-moving-to-trash t
 window-combination-resize t
 delete-trailing-lines t
 x-stretch-cursor t)

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; 全局开启一些模式
(setq-default abbrev-mode t)
(display-time-mode 1)                           ; 在 mode-line 中显示时间
(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))                     ; 显示电量
(global-subword-mode 1)                         ; Iterate through CamelCase words
;; (prettier-js-mode 1)
;; (delete-selection-mode 1)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(defun maybe-use-prettier ()
  "Enable prettier-js-mode if an rc file is located."
  (if (locate-dominating-file default-directory ".prettierrc")
      (prettier-js-mode +1)))
(add-hook 'typescript-mode-hook 'maybe-use-prettier)
(add-hook 'js2-mode-hook 'maybe-use-prettier)
(add-hook 'web-mode-hook 'maybe-use-prettier)
(add-hook 'rjsx-mode-hook 'maybe-use-prettier)

(defadvice! +literate-tangle-async-h ()
  "A very simplified version of `+literate-tangle-h', but async."
  :override #'+literate-tangle-h
  (let ((default-directory doom-private-dir))
    (async-shell-command
     (format "emacs --batch --eval \"(progn \
(require 'org) (setq org-confirm-babel-evaluate nil) \
(org-babel-tangle-file \\\"%s\\\"))\" \
&& /bin/bash ~/.gclrc/shl/cp-config-org.sh"
             +literate-config-file))))

(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

(setq doom-theme 'doom-vibrant)

(setq doom-font (font-spec :family "JetBrains Mono" :size 16))

(use-package! valign
  :custom
  (valign-fancy-bar t)
  :hook
  (org-mode . valign-mode))

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

(use-package deft
  :after org
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension '("org" "md" "txt"))
  (deft-directory "~/github/documents"))

(defalias 'ex! 'evil-ex-define-cmd)

;; 快捷操作，通过 : 冒号进入 evil 命令模式
;; File operations
(ex! "cp"          #'+evil:copy-this-file)
(ex! "mv"          #'+evil:move-this-file)
(ex! "rm"          #'+evil:delete-this-file)

;; window 操作
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(after! leetcode
  (setq leetcode-prefer-language "javascript"
        leetcode-prefer-sql "mysql"
        leetcode-save-solutions t
        leetcode-directory "~/github/make-leetcode"))

(use-package! lsp-ui
  :commands
  lsp-ui-mode
  :config
  (setq lsp-headerline-breadcrumb-enable t ; 左上角显示文件路径
        lsp-lens-enable t                  ; 显示被引用次数
        ))

(use-package! company-lsp
  :commands company-lsp)

(use-package! lsp-mode
  :hook (
         (web-mode . lsp)
         (typescript-mode . lsp)
         (rjsx-mode . lsp)
         (java-mode . lsp)
         (javascript-mode . lsp)
         (js2-mode . lsp)
         (python-mode . lsp)
         (go-mode . lsp)
         (css-mode . lsp)
         )
  :commands lsp
  :config
  (setq lsp-idle-delay 0.500
        lsp-enable-file-watchers nil))

(use-package! lsp-java
  :config (add-hook 'java-mode-hook 'lsp))
(use-package! dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))
(use-package! dap-java
  :ensure nil)

;; 关闭自动格式化，全局关闭
;; (setq +form-with-lsp nil)
;; 指定模式
(setq-hook! 'typescript-mode-hook +format-with-lsp nil)
(setq-hook! 'typescript-tsx-mode-hook +format-with-lsp nil)

(add-hook 'org-mode-hook
          (lambda () (display-line-numbers-mode -1)))

(use-package! org-fancy-priorities
  :diminish
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴")))

(use-package! org-pretty-tags
  :diminish org-pretty-tags-mode
  :ensure t
  :config
  (setq org-pretty-tags-surrogate-strings
        '(("work"  . "⚒")))

  (org-pretty-tags-global-mode))

(use-package! parrot
  :config
  (parrot-mode))

(setq parrot-rotate-dict
      '(
        (:rot ("alpha" "beta") :caps t :lower nil)
        ;; => rotations are "Alpha" "Beta"

        (:rot ("snek" "snake" "stawp"))
        ;; => rotations are "snek" "snake" "stawp"

        (:rot ("yes" "no") :caps t :upcase t)
        ;; => rotations are "yes" "no", "Yes" "No", "YES" "NO"

        (:rot ("&" "|"))
        ;; => rotations are "&" "|"
        ;; default dictionary starts here ('v')
        (:rot ("begin" "end") :caps t :upcase t)
        (:rot ("enable" "disable") :caps t :upcase t)
        (:rot ("enter" "exit") :caps t :upcase t)
        (:rot ("forward" "backward") :caps t :upcase t)
        (:rot ("front" "rear" "back") :caps t :upcase t)
        (:rot ("get" "set") :caps t :upcase t)
        (:rot ("high" "low") :caps t :upcase t)
        (:rot ("in" "out") :caps t :upcase t)
        (:rot ("left" "right") :caps t :upcase t)
        (:rot ("min" "max") :caps t :upcase t)
        (:rot ("on" "off") :caps t :upcase t)
        (:rot ("prev" "next"))
        (:rot ("start" "stop") :caps t :upcase t)
        (:rot ("true" "false") :caps t :upcase t)
        (:rot ("&&" "||"))
        (:rot ("==" "!="))
        (:rot ("===" "!=="))
        (:rot ("." "->"))
        (:rot ("if" "else" "elif"))
        (:rot ("ifdef" "ifndef"))
        ;; javascript
        (:rot ("var" "let" "const"))
        (:rot ("null" "undefined"))
        (:rot ("number" "object" "string" "symbol"))

        ;; c/...
        (:rot ("int8_t" "int16_t" "int32_t" "int64_t"))
        (:rot ("uint8_t" "uint16_t" "uint32_t" "uint64_t"))
        (:rot ("1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))
        (:rot ("1st" "2nd" "3rd" "4th" "5th" "6th" "7th" "8th" "9th" "10th"))

        ;; org
        (:rot ("DONE" "DOING" "WAITING" "PENDING"))
        (:rot ("increment", "decrement"))

        ))

;; 出文本模式下，开启拼写检查
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))

(after! text-mode
  (add-hook! 'text-mode-hook
             ;; Apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max)))))

;; ranger
(after! ranger
  :config
  (setq ranger-show-literal nil))

(use-package! visual-regexp
  :commands (vr/select-replace vr/select-query-replace))

(use-package! visual-regexp-steriods
  :commands (vr/select-replace vr/select-query-replace))

(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))

(setq treemacs-file-ignore-extensions
      '(;; LaTeX
        "aux"
        "ptc"
        "fdb_latexmk"
        "fls"
        "synctex.gz"
        "toc"
        ;; LaTeX - glossary
        "glg"
        "glo"
        "gls"
        "glsdefs"
        "ist"
        "acn"
        "acr"
        "alg"
        ;; LaTeX - pgfplots
        "mw"
        ;; LaTeX - pdfx
        "pdfa.xmpi"
        ))
(setq treemacs-file-ignore-globs
      '(;; LaTeX
        "*/_minted-*"
        ;; AucTeX
        "*/.auctex-auto"
        "*/_region_.log"
        "*/_region_.tex"))

;; web-mode hydra
(defhydra hydra-web-mode (:color blue :quit-key "q" :hint nil)
  "
^Element^                       ^Element^                       ^Attribute^             ^Block
^^^^^^^^---------------------------------------------------------------------------------------------
_a_ : Select content            _r_ : Rename                    _0_ : Start             _<_ : Begin
_b_ : Start                     _s_ : Select                    _9_ : End               _>_ : End
_c_ : Clone                     _t_ : Move Down                 _*_ : Insert            _-_ : Select
_e_ : End                       _u_ : Parent                    _N_ : Next
_f_ : Fold/unfold children      _v_ : Delete without content    _P_ : Previous                  _k_
_i_ : Insert                    _w_ : Wrap Element              _S_ : Select                _h_      _l_
_I_ : Insert cursor             _t_ : Last(open/close)          _X_ : Delete                    _j_
_K_ : Delete                    _T_ : Next(open/close)          _M_ : Match tag
_n_ : Next                      _._ : Wrap Markup               _A_ : Sort
_p_ : Previous
"
  ("a" web-mode-element-content-select)
  ("b" web-mode-element-beginning :exit nil)
  ("c" web-mode-element-clone)
  ("e" web-mode-element-end :exit nil)
  ("f" web-mode-element-children-fold-or-unfold :exit nil)
  ("F" web-mode-fold-unfold :exit nil)
  ("i" web-mode-element-insert)
  ("I" web-mode-element-insert-at-point)
  ("K" web-mode-element-kill)
  ("m" web-mode-element-mute-blanks)
  ("n" web-mode-element-next :color "pink" :exit nil)
  ("p" web-mode-element-previous :color "pink" :exit nil)
  ("r" web-mode-element-rename)
  ("s" web-mode-element-select)
  ("t" web-mode-element-transpose)
  ("u" web-mode-element-parent :color "pink" :exit nil)
  ("v" web-mode-element-vanish)
  ("w" web-mode-element-wrap)
  ("t" web-mode-tag-previous :color "pink" :exit nil)
  ("T" web-mode-tag-next :color "pink" :exit nil)
  ("." emmet-wrap-with-markup)
  ("q" nil "quit" :exit t)
  ;; attribute
  ("0" web-mode-attribute-beginning :exit nil)
  ("9" web-mode-attribute-end :exit nil)
  ("*" web-mode-attribute-insert)
  ("X" web-mode-attribute-kill)
  ("A" web-mode-tag-attributes-sort :exit nil)
  ("K" web-mode-element-kill)
  ("M" web-mode-tag-match :exit nil :color "pink")
  ("N" web-mode-attribute-next :exit nil :color "pink")
  ("P" web-mode-attribute-previous :exit nil :color "pink")
  ("S" web-mode-attribute-select)
  ;; block
  ("<" web-mode-block-next :exit nil :color "pink")
  (">" web-mode-block-previous :exit nil :color "pink")
  ("-" web-mode-block-select)
  ;; movement
  ("j" next-line :exit nil :color "blue")
  ("k" previous-line :exit nil :color "blue")
  ("h" backward-char :exit nil :color "blue")
  ("l" forward-char :exit nil :color "blue")
  )

(setq which-key-idle-delay 0.5)

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

(setq yas-triggers-in-field t)


(use-package! doom-snippets             ; hlissner
  :after yasnippet)

(use-package! yasnippet-snippets        ; AndreaCrotti
  :after yasnippet)

;; hungry delete
(use-package! smart-hungry-delete
  :ensure t
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks
  :config (smart-hungry-delete-add-default-hooks)
  )

;; multiple cursors hydra
(defhydra hydra-multiple-cursors (:color blue :hint nil)
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("s" mc/mark-all-in-region-regexp :exit t)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil))

(use-package! scrollkeeper)
(global-set-key [remap scroll-up-command] #'scrollkeeper-contents-up)
(global-set-key [remap scroll-down-command] #'scrollkeeper-contents-down)

;; web 开发配置
(setq css-indent-offset 2
      js2-basic-offset 2
      js-switch-indent-offset 2
      js-indent-level 2
      js2-mode-show-parse-errors nil
      js2-mode-show-strict-warnings nil
      web-mode-attr-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-markup-indent-offset 2
      web-mode-enable-current-element-highlight t
      web-mode-enable-current-column-highlight t)
(setq-default typescript-indent-level 2)

(use-package! rjsx-mode)
(use-package! import-js
  :defer t
  :init
  (add-hook! (js2-mode rjsx-mode) (run-import-js))
  (add-hook! (js2-mode rjsx-mode)
    (add-hook 'after-save-hook #'import-js-fix nil t)))
(advice-add '+javascript|cleanup-tide-processes :after 'kill-import-js)

;; fix true/false symbols for javascript
;; (setq +pretty-code-enabled-modes nil)
;; or
;; (remove-hook 'after-change-major-mode-hook #'+pretty-code-init-pretty-symbols-h)
;; (defun setup-js2-prettify-symbols ()
;;   "Set prettify symbols alist."
;;   (interactive)
;;   (setq prettify-symbols-alist '(("lambda" . "λ")
;;                                  ("->" . "→")
;;                                  ("!=" . "≠")
;;                                  ("<=" . "≤")
;;                                  (">=" . "≥")
;;                                  ("=<<" . "=≪")
;;                                  ("!" . "￢")
;;                                  ("null" . "∅")
;;                                  ("function" . "ƒ")
;;                                  (">>=" . "≫=")))
;;   (delete '("false" . "𝔽") prettify-symbols-alist)
;;   (delete '("true" . "𝕋") prettify-symbols-alist)
;;   (prettify-symbols-mode -1)
;;   )

;; (add-hook! 'js2-mode-hook 'setup-js2-prettify-symbols)
