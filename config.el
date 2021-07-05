;;;###autoload
(defun gcl/use-eslint-from-node-modules ()
    "Set local eslint if available."
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

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

(defun gcl/async-shell-command-silently (command)
  "async shell command silently."
  (interactive)
  (let
      ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*.*"
          (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command)))

;;; config.el -*- lexical-binding: t; -*-

(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "C-'") 'imenu-list-smart-toggle)

(map! :niv      "C-s" nil
      :niv      "C-d" nil
      :niv      "C-i" nil
      :niv      "M-," nil
      :niv      "M-." nil

      "C-'" nil

      :leader
      "A" nil
      "X" nil
      "/" nil

      ;; remap
      [remap evil-undo] #'undo-tree-undo)

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
      "s-i"     #'gcl/string-inflection-cycle-auto)
      ;; "s-("     #'sp-backward-barf-sexp
      ;; "s-)"     #'sp-forward-barf-sexp)

(map!
 "C-:"     #'avy-goto-char
 "C-;"     #'avy-goto-char-2
 "C-s"     #'+default/search-buffer

 ;; smartparen
 ;; "C-("     #'sp-backward-slurp-sexp
 ;; "C-)"     #'sp-forward-slurp-sexp

 ;; C-c
 "C-c a c"     #'org-mac-chrome-insert-frontmost-url
 "C-c d"       #'insert-current-date-time
 "C-c t"       #'insert-current-time
 "C-c o"       #'crux-open-with
 "C-c r"       #'vr/replace
 "C-c q"       #'vr/query-replace
 "C-c u"       #'crux-view-url
 "C-c y"       #'youdao-dictionary-search-at-point+

 ;; C-c l
 "C-c l o"      #'link-hint-open-link
 "C-c l c"      #'link-hint-copy-link
 "C-c l a"      #'link-hint-open-link-at-point
 "C-c l C"      #'link-hint-copy-link-at-point

 "C-a"          #'crux-move-beginning-of-line
 :niv      "C-e"     #'evil-end-of-line
 :niv      "C-="     #'er/expand-region
 )

(map! "M--"     #'gcl/goto-match-paren
      "M-i"     #'parrot-rotate-next-word-at-point
      "M-f"     #'scroll-up-command)

(global-set-key (kbd "M-f") 'pyim-forward-word)
(global-set-key (kbd "M-b") 'pyim-backward-word)

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

(defadvice! +literate-tangle-async-h ()
  "A very simplified version of `+literate-tangle-h', but async."
  :override #'+literate-tangle-h
  (let ((default-directory doom-private-dir))
    (gcl/async-shell-command-silently (format "emacs --batch --eval \"(progn \
(require 'org) (setq org-confirm-babel-evaluate nil) \
(org-babel-tangle-file \\\"%s\\\"))\" \
&& /bin/bash ~/.gclrc/shl/cp-config-org.sh"
             +literate-config-file))))

;; 启动全屏
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

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

;; 开启模式
(global-undo-tree-mode 1)

(setq doom-theme 'doom-vibrant)

;; (setq doom-font (font-spec :family "JetBrains Mono" :size 16))
(setq doom-font (font-spec :family "Fira Code" :size 16))

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

(after! avy
  ;; home row priorities: 8 6 4 5 - - 1 2 3 7
  (setq avy-keys '(?n ?e ?i ?s ?t ?r ?i ?a)))

(use-package! color-rg
  :commands (color-rg-search-input
             color-rg-search-symbol
             color-rg-search-input-in-project)
  :bind
  (:map isearch-mode-map
   ("M-s M-s" . isearch-toggle-color-rg)))

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

;; (when (display-graphic-p)
;;   (use-package! eaf
;;     :if (eq system-type 'gnu/linux)
;;     :custom
;;     (eaf-find-alternate-file-in-dired t)
;;     :config
;;     (add-hook! 'eaf-mode-hook 'xah-fly-keys-off)

;;     (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;     (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)

;;     (defun eaf-open-google ()
;;       "Open Google using EAF."
;;       (interactive)
;;       (eaf-open-browser "https://www.google.com")))

(defalias 'ex! 'evil-ex-define-cmd)

;; 快捷操作，通过 : 冒号进入 evil 命令模式
;; File operations
(ex! "cp"          #'+evil:copy-this-file)
(ex! "mv"          #'+evil:move-this-file)
(ex! "rm"          #'+evil:delete-this-file)

;; window 操作
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(use-package! maple-iedit
   :commands (maple-iedit-match-all maple-iedit-match-next maple-iedit-match-previous)
   :config
   (delete-selection-mode t)
   (setq maple-iedit-ignore-case t)
   (defhydra maple/iedit ()
     ("n" maple-iedit-match-next "next")
     ("t" maple-iedit-skip-and-match-next "skip and next")
     ("T" maple-iedit-skip-and-match-previous "skip and previous")
     ("p" maple-iedit-match-previous "prev"))
   :bind (:map evil-visual-state-map
          ("n" . maple/iedit/body)
          ("C-n" . maple-iedit-match-next)
          ("C-p" . maple-iedit-match-previous)
          ("C-t" . map-iedit-skip-and-match-next)
          ("C-T" . map-iedit-skip-and-match-previous)))

(setq golden-ratio-exclude-modes
      '("calendar-mode"
        "org-agenda-mode"
        "help-mode"
        "dired-mode"
        "ranger-mode"
        "helpful-mode"
        "rxt-help-mode"
        "treemacs-mode" ))
  (setq golden-ratio-exclude-buffer-names
      '("*Org tags*"
        "*Org todo*"
        "*info*"
        "*Messages*"))

(use-package! golden-ratio
  :after-call pre-command-hook
  :config
  (golden-ratio-mode +1)
  ;; Using this hook for resizing windows is less precise than
  ;; `doom-switch-window-hook'.
  (remove-hook 'window-configuration-change-hook #'golden-ratio)
  (add-hook 'doom-switch-window-hook #'golden-ratio))

(use-package! good-scroll
  :config
  (good-scroll-mode 1))
(global-set-key [remap evil-scroll-up] #'good-scroll-up)
(global-set-key [remap evil-scroll-down] #'good-scroll-down)
(global-set-key [remap evil-scroll-page-up] #'good-scroll-up-full-screen)
(global-set-key [remap evil-scroll-page-down] #'good-scroll-down-full-screen)

(use-package! link-hint
  :config
  (setq browse-url-browser-function 'browse-url-chromium
        browse-url-generic-args '("--target" "tab")))

(use-package! visual-regexp
  :commands (vr/select-replace vr/select-query-replace))

(use-package! visual-regexp-steriods
  :commands (vr/select-replace vr/select-query-replace))

;; (defvar +my-ext-dir (expand-file-name "~/.doom.d/extensions"))
;; (setq-default pyim-english-input-switch-functions
;;               '(pyim-probe-dynamic-english
;;                 pyim-probe-isearch-mode
;;                 pyim-probe-program-mode
;;                 pyim-probe-org-structure-template))
;; (setq-default pyim-punctuation-half-width-functions
;;               '(pyim-probe-punctuation-line-beginning
;;                 pyim-probe-punctuation-after-punctuation))

;; (use-package! pyim
;;   :demand t
;;   :defer 1
;;   :diminish pyim-isearch-mode
;;   :init
;;   (setq default-input-method "pyim"
;;         pyim-title "ㄓ"
;;         pyim-default-scheme 'rime
;;         pyim-page-length 7
;;         pyim-page-tooltip 'posframe) ;;'popup) ;;proframe)

;;   :config
;;   (setq-default pyim-english-input-switch-functions
;;                 '(pyim-probe-dynamic-english
;;                   pyim-probe-evil-normal-mode
;;                   pyim-probe-program-mode
;;                   pyim-probe-org-structure-template))

;;   (setq-default pyim-punctuation-half-width-functions
;;                 '(pyim-probe-punctuation-line-beginning
;;                   pyim-probe-punctuation-after-punctuation)))

;; (defvar liberime-is-loaded nil)

;; (use-package! liberime
;;   :when (featurep! +rime)
;;   :load-path (lambda()(expand-file-name "liberime" +my-ext-dir))
;;   :defer 1
;;   :unless liberime-is-loaded
;;   :custom
;;   (rime_share_data_dir "/Library/Input Methods/Squirrel.app/Contents/SharedSupport/")
;;   (rime_user_data_dir (expand-file-name "rime" +my-ext-dir))
;;   :init
;;   (module-load (expand-file-name "liberime.so" +my-ext-dir))
;;   :config
;;   (setq liberime-is-loaded t)
;;   (liberime-start rime_share_data_dir rime_user_data_dir)
;;   (liberime-select-schema  "wubi_pinyin")) ;;"wubi_pinyin"))  luna_pinyin_simp"))

;; ;; 中英文之间添加空格
;; (use-package! pangu-spacing
;;   :hook (text-mode . pangu-spacing-mode)
;;   :config
;;   ;; Always insert `real' space in org-mode .
;;   (setq-hook! 'org-mode-hook pangu-spacing-real-insert-separtor t))

;; (use-package! fcitx
;;   :after evil
;;   :config
;;   (when (executable-find "fcitx-remote")
;;     (fcitx-evil-turn-on)))

;; (use-package! ace-pinyin
;;   :after avy
;;   :init (setq ace-pinyin-use-avy t)
;;   :config (ace-pinyin-global-mode t))

;;; Hacks

;; (defun +chinese*org-html-paragraph (paragraph contents info)
;;   "Join consecutive Chinese lines into a single long line without unwanted space
;; when exporting org-mode to html."
;;   (let* ((fix-regexp "[[:multibyte:]]")
;;          (origin-contents contents)
;;          (fixed-contents
;;           (replace-regexp-in-string
;;            (concat "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)")
;;            "\\1\\2"
;;            origin-contents)))
;;     (list paragraph fixed-contents info)))
;; (advice-add #'org-html-paragraph :filter-args #'+chinese*org-html-paragraph)

(use-package! js-doc
  :bind (:map js2-mode-map
         ("C-c i" . js-doc-insert-function-doc)
         ("@" . js-doc-insert-tag))
  :config
  (setq js-doc-mail-address user-mail-address
       js-doc-author (format "%s<%s>" user-full-name js-doc-mail-address)
       js-doc-url user-blog-url
       js-doc-license "MIT"))

(use-package! flycheck
    :config
    (add-hook 'after-init-hook 'global-flycheck-mode)
    (add-hook 'flycheck-mode-hook 'gcl/use-eslint-from-node-modules))

(after! leetcode
  (setq leetcode-prefer-language "javascript"
        leetcode-prefer-sql "mysql"
        leetcode-save-solutions t
        leetcode-directory "~/github/make-leetcode"))

(use-package! lsp-mode
  :hook ((web-mode . lsp)
         (rjsx-mode . lsp)
         (typescript-mode . lsp)
         ;; (vue-mode . lsp)
         (python-mode . lsp)
         (go-mode . lsp)
         (css-mode . lsp)
         (js2-mode . lsp))
  :commands lsp
  :config
  (setq lsp-idle-delay 0.2
        lsp-enable-file-watchers nil))

(use-package! lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-headerline-breadcrumb-enable t ; 左上角显示文件路径
        lsp-lens-enable t                  ; 显示被引用次数
        ))

;; 关闭自动格式化，全局关闭
;; (setq +form-with-lsp nil)
;; 指定模式
;; (setq-hook! 'typescript-mode-hook +format-with-lsp nil)
;; (setq-hook! 'typescript-tsx-mode-hook +format-with-lsp nil)

(use-package! git-gutter
  :config
  (global-git-gutter-mode 't))

(add-hook 'org-mode-hook
          (lambda () (display-line-numbers-mode -1)))

;; (org-agenda-files  `(,(expand-file-name "agenda.org" org-directory)))
;; 自动隐藏 */~= 符号
;; (org-hide-emphasis-markers t)
;; (org-module  '(org-habit org-checklist))

(use-package! org-fancy-priorities
  :diminish
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴")))

(use-package! org-pretty-tags
  :diminish org-pretty-tags-mode
  :config
  (setq org-pretty-tags-surrogate-strings
        '(("work"  . "⚒")))

  (org-pretty-tags-global-mode))

(use-package! valign
  :custom
  (valign-fancy-bar t)
  :hook
  (org-mode . valign-mode))

;; (setq org-roam-directory "~/.doom.d/.local/roam/")
;; (use-package org-roam-server
;;   :after (org-roam server)
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8078
;;         org-roam-server-export-inline-images t
;;         org-roam-server-authenticate nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20)
;;   (defun org-roam-server-open ()
;;     "Ensure the server is active, then open the roam graph."
;;     (interactive)
;;     (org-roam-server-mode 1)
;;     (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))))

(use-package! org-ol-tree
  :commands org-ol-tree)
(map! :map org-mode-map
      :after org
      :localleader
      :desc "Outline" "O" #'org-ol-tree)

(use-package! engrave-faces-latex
  :after ox-latex)

(use-package! ox-gfm
  :after org)

(use-package! org-pandoc-import
  :after org)

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(org-link-set-parameters "yt" :export #'+org-export-yt)
(defun +org-export-yt (path desc backend _com)
  (cond ((org-export-derived-backend-p backend 'html)
         (format "<iframe width='440' \
height='335' \
src='https://www.youtube.com/embed/%s' \
frameborder='0' \
allowfullscreen>%s</iframe>" path (or "" desc)))
        ((org-export-derived-backend-p backend 'latex)
         (format "\\href{https://youtu.be/%s}{%s}" path (or desc "youtube")))
        (t (format "https://youtu.be/%s" path))))

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

;; ranger true
(after! ranger
  :config
  (setq ranger-show-literal nil))

(setq yas-triggers-in-field t)

(use-package! doom-snippets             ; hlissner
  :after yasnippet)

(use-package! yasnippet-snippets        ; AndreaCrotti
  :after yasnippet)

(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))

(use-package! smartparens
  :init
  (map! :map smartparens-mode-map
       "C-)" #'sp-forward-slurp-sexp
       "C-(" #'sp-forward-barf-sexp
       "C-{" #'sp-backward-slurp-sexp
       "C-}" #'sp-backward-barf-sexp
       ))

;; (add-transient-hook! 'prog-mode-hook
;;  (require 'tree-sitter-langs)
;;  (global-tree-sitter-mode))

;; (add-hook! 'tree-sitter-after-on-hook
;;          #'tree-sitter-hl-mode)

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

;; (use-package! rjsx-mode)

(defun maybe-use-prettier ()
  "Enable prettier-js-mode if an rc file is located."
  (if (locate-dominating-file default-directory ".prettierrc")
      (prettier-js-mode +1)))
(add-hook 'typescript-mode-hook 'maybe-use-prettier)
(add-hook 'js2-mode-hook 'maybe-use-prettier)
(add-hook 'web-mode-hook 'maybe-use-prettier)
(add-hook 'rjsx-mode-hook 'maybe-use-prettier)

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

(after! which-key
  (setq! which-key-idle-delay 0.1
         which-key-idle-secondary-delay 0.2))

(use-package! dotenv-mode
  :mode ("\\.env\\.?.*\\'" . dotenv-mode))

;; (use-package! zoom
;;   :hook ((doom-first-input . zoom-mode))
;;   :config
;;   (setq zoom-size '(0.8 . 0.8)
;;         zoom-ignored-major-modes '(dired-mode vterm-mode help-mode helpful-mode rxt-help-mode help-mode-menu org-mode)
;;         zoom-ignored-buffer-names '("*doom:scratch*" "*info*" "*helpful variable: argv*")
;;         zoom-ignored-buffer-name-regexps '("^\\*calc" "\\*helpful variable: .*\\*")
;;         zoom-ignore-predicates (list (lambda () (> (count-lines (point-min) (point-max)) 20)))))
