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
      ;; [remap evil-undo] #'undo-tree-undo
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

      "C-c e e"        #'all-the-icons-insert
      "C-c e a"        #'all-the-icons-insert-faicon
      "C-c e f"        #'all-the-icons-insert-fileicon
      "C-c e w"        #'all-the-icons-insert-wicon
      "C-c e o"        #'all-the-icons-insert-octicon
      "C-c e m"        #'all-the-icons-insert-material
      "C-c e i"        #'all-the-icons-insert-alltheicon
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

;;; config.el -*- lexical-binding: t; -*-

;; (setq package--init-file-ensured t
;;       package-user-dir (expand-file-name "elpa" doom-packages-dir)
;;       package-gnupghome-dir (expand-file-name "gpg" doom-packages-dir)
;;       package-enable-at-startup nil
;;       package-archives
;;       '(("melpa" . "http://elpa.emacs-china.org/melpa/")))

;; 启动全屏
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; 个人信息配置
(setq user-full-name "Zhicheng Lee"
      user-mail-address "gccll.love@gmail.com"
      user-blog-url "https://www.cheng92.com")

;; setq, set-default 统一配置的地方
(setq read-process-output-max (* 1024 1024)) ;; 1mb
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
;; (global-undo-tree-mode 1)

;;; config.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-vibrant)

;; (setq doom-font (font-spec :family "JetBrains Mono" :size 16))
(setq doom-font (font-spec :family "Fira Code" :size 16))

(setq frame-title-format
      '(""
        ;; (:eval
        ;;  (if (s-contains-p org-roam-directory (or buffer-file-name ""))
        ;;      (replace-regexp-in-string
        ;;       ".*/[0-9]*-?" "☰ "
        ;;       (subst-char-in-string ?_ ?  buffer-file-name))
        ;;    "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

;;; config.el -*- lexical-binding: t; -*-

(after! avy
  ;; home row priorities: 8 6 4 5 - - 1 2 3 7
  (setq avy-keys '(?n ?e ?i ?s ?t ?r ?i ?a)))

(map!
 "C-c b d"      #'bm-remove-all-current-buffer
 "C-c b D"      #'bm-remove-all-all-buffers
 "C-c b n"      #'bm-next
 "C-c b p"      #'bm-previous
 "C-c b t"      #'bm-toggle
 "C-c b l"      #'bm-show-all
 "C-c b s"      #'adq/bm-save)

(use-package! bm
   :demand t
   :init
   (setq bm-restore-repository-on-load t)
   :config

   (bind-keys
    :map bm-show-mode-map
    ("j" . next-line)
    ("k" . previous-line))

   (setq bm-cycle-all-buffers t
         bm-highlight-style 'bm-highlight-only-fringe
         bm-repository-size 1000)
   (setq-default bm-buffer-persistence t)

   (defun adq/bm-save ()
     "Save bookmarks to persistent repository."
     (interactive)
     (bm-buffer-save-all)
     (bm-repository-save))

   (advice-add 'bm-bookmark-add
               :after (lambda (&rest args)
                        (adq/bm-save)))
   (advice-add 'bm-bookmark-remove
               :after (lambda (&rest args)
                        (adq/bm-save)))
   (add-hook 'after-init-hook #'bm-repository-load)
   (add-hook 'find-file-hook #'bm-buffer-restore)
   (add-hook 'after-rever-hook #'bm-buffer-restore)
   (add-hook 'kill-buffer-hook #'bm-buffer-save)
   (add-hook 'after-save-hook #'bm-buffer-save)
   (add-hook 'kill-emacs-hook
             (lambda ()
               (bm-buffer-save-all)
               (bm-repository-save))))

(setq calc-angle-mode 'rad  ; radians are rad
      calc-symbolic-mode t) ; keeps expressions like \sqrt{2} irrational for as long as possible

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
  ;; (setq company-show-numbers t)
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

(use-package! emacs-everywhere
  :if (daemonp)
  :config
  (require 'spell-fu)
  (setq emacs-everywhere-major-mode-function #'org-mode
        emacs-everywhere-frame-name-format "Edit ∷ %s — %s")
  (defadvice! emacs-everywhere-raise-frame ()
    :after #'emacs-everywhere-set-frame-name
    (setq emacs-everywhere-frame-name (format emacs-everywhere-frame-name-format
                                (emacs-everywhere-app-class emacs-everywhere-current-app)
                                (truncate-string-to-width
                                 (emacs-everywhere-app-title emacs-everywhere-current-app)
                                 45 nil nil "…")))
    ;; need to wait till frame refresh happen before really set
    (run-with-timer 0.1 nil #'emacs-everywhere-raise-frame-1))
  (defun emacs-everywhere-raise-frame-1 ()
    (call-process "wmctrl" nil nil nil "-a" emacs-everywhere-frame-name)))

(defalias 'ex! 'evil-ex-define-cmd)

;; 快捷操作，通过 : 冒号进入 evil 命令模式
;; File operations
(ex! "cp"          #'+evil:copy-this-file)
(ex! "mv"          #'+evil:move-this-file)
(ex! "rm"          #'+evil:delete-this-file)

;; window 操作
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(use-package! highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray"))

(use-package! hungry-delete
  :config
  (add-hook! 'after-init-hook #'global-hungry-delete-mode))

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
;; (global-set-key [remap evil-scroll-up] #'good-scroll-up)
;; (global-set-key [remap evil-scroll-down] #'good-scroll-down)
;; (global-set-key [remap evil-scroll-page-up] #'good-scroll-down-full-screen)
;; (global-set-key [remap evil-scroll-page-down] #'good-scroll-up-full-screen)

(use-package! link-hint
  :config
  (setq browse-url-browser-function 'browse-url-chromium
        browse-url-generic-args '("--target" "tab")))

(use-package! visual-regexp
  :commands (vr/select-replace vr/select-query-replace))

(use-package! visual-regexp-steriods
  :commands (vr/select-replace vr/select-query-replace))

(use-package! ein
  :config
  (setq ob-ein-languages
   (quote
    (("ein-python" . python)
     ("ein-R" . R)
     ("ein-r" . R)
     ("ein-rust" . rust)
     ("ein-haskell" . haskell)
     ("ein-julia" . julia))))
  )

(after! ein:ipynb-mode                  ;
  (poly-ein-mode 1)
  (hungry-delete-mode -1)
  )

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

(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

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

(use-package! counsel-tramp
  :config
  (setq tramp-default-method "sshx")
  (setq make-backup-files nil)
  (setq create-lockfiles nil)
  ;; (setq counsel-tramp-custom-connections
  ;;       '(/ssh:dev|sudo:root@192.168.88.158:/var/www/html/lzc))
  (setq counsel-tramp-localhost-directory "/var/www/html")
  ;; (add-hook 'counsel-tramp-pre-command-hook '(lambda () (projectile-mode 0)
  ;;                                              (editorconfig-mode 0)))
  ;; (add-hook 'counsel-tramp-quit-hook '(lambda () (projectile-mode 1)
  ;;                                       (editorconfig-mode 1)))
  )
(define-key global-map (kbd "C-c s") 'counsel-tramp)

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

;; 去掉 evilem-... 开头的项
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

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

;; [[file:config.org::*Elfeed^{Failed}][Elfeed^{Failed}:1]]
(setq rmh-elfeed-org-files '("~/.gclrc/org/elfeed.org"))
(use-package! visual-fill-column
  :after org)
;; Elfeed^{Failed}:1 ends here

;; [[file:config.org::*Keybindings][Keybindings:1]]
(map! :map elfeed-search-mode-map
      :after elfeed-search
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :n "q" #'+rss/quit
      :n "e" #'elfeed-update
      :n "r" #'elfeed-search-untag-all-unread
      :n "u" #'elfeed-search-tag-all-unread
      :n "s" #'elfeed-search-live-filter
      :n "RET" #'elfeed-search-show-entry
      :n "p" #'elfeed-show-pdf
      :n "+" #'elfeed-search-tag-all
      :n "-" #'elfeed-search-untag-all
      :n "S" #'elfeed-search-set-filter
      :n "b" #'elfeed-search-browse-url
      :n "y" #'elfeed-search-yank)
(map! :map elfeed-show-mode-map
      :after elfeed-show
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :nm "q" #'+rss/delete-pane
      :nm "o" #'ace-link-elfeed
      :nm "RET" #'org-ref-elfeed-add
      :nm "n" #'elfeed-show-next
      :nm "N" #'elfeed-show-prev
      :nm "p" #'elfeed-show-pdf
      :nm "+" #'elfeed-show-tag
      :nm "-" #'elfeed-show-untag
      :nm "s" #'elfeed-show-new-live-search
      :nm "y" #'elfeed-show-yank)
;; Keybindings:1 ends here

;; [[file:config.org::*可用性][可用性:1]]
(after! elfeed-search
  (set-evil-initial-state! 'elfeed-search-mode 'normal))
(after! elfeed-show-mode
  (set-evil-initial-state! 'elfeed-show-mode   'normal))

(after! evil-snipe
  (push 'elfeed-show-mode   evil-snipe-disabled-modes)
  (push 'elfeed-search-mode evil-snipe-disabled-modes))
;; 可用性:1 ends here

;; [[file:config.org::*视图效果][视图效果:1]]
(after! elfeed

  (elfeed-org)
  (use-package! elfeed-link)

  (setq elfeed-search-filter "@1-week-ago +unread"
        elfeed-search-print-entry-function '+rss/elfeed-search-print-entry
        elfeed-search-title-min-width 80
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'+rss/delete-pane
        elfeed-show-refresh-function #'+rss/elfeed-show-refresh--better-style
        shr-max-image-proportion 0.6)

  (add-hook! 'elfeed-show-mode-hook (hide-mode-line-mode 1))
  (add-hook! 'elfeed-search-update-hook #'hide-mode-line-mode)

  (defface elfeed-show-title-face '((t (:weight ultrabold :slant italic :height 1.5)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (defface elfeed-show-author-face `((t (:weight light)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (set-face-attribute 'elfeed-search-title-face nil
                      :foreground 'nil
                      :weight 'light)

  (defadvice! +rss-elfeed-wrap-h-nicer ()
    "Enhances an elfeed entry's readability by wrapping it to a width of
`fill-column' and centering it with `visual-fill-column-mode'."
    :override #'+rss-elfeed-wrap-h
    (setq-local truncate-lines nil
                shr-width 120
                visual-fill-column-center-text t
                default-text-properties '(line-height 1.1))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (visual-fill-column-mode)
      ;; (setq-local shr-current-font '(:family "Merriweather" :height 1.2))
      (set-buffer-modified-p nil)))

  (defun +rss/elfeed-search-print-entry (entry)
    "Print ENTRY to the buffer."
    (let* ((elfeed-goodies/tag-column-width 40)
           (elfeed-goodies/feed-source-column-width 30)
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat (mapconcat 'identity tags ",")))
           (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                           elfeed-goodies/tag-column-width 4))

           (tag-column (elfeed-format-column
                        tags-str (elfeed-clamp (length tags-str)
                                               elfeed-goodies/tag-column-width
                                               elfeed-goodies/tag-column-width)
                        :left))
           (feed-column (elfeed-format-column
                         feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width)
                         :left)))

      (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
      (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
      (insert (propertize title 'face title-faces 'kbd-help title))
      (setq-local line-spacing 0.2)))

  (defun +rss/elfeed-show-refresh--better-style ()
    "Update the buffer to match the selected entry, using a mail-style."
    (interactive)
    (let* ((inhibit-read-only t)
           (title (elfeed-entry-title elfeed-show-entry))
           (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
           (author (elfeed-meta elfeed-show-entry :author))
           (link (elfeed-entry-link elfeed-show-entry))
           (tags (elfeed-entry-tags elfeed-show-entry))
           (tagsstr (mapconcat #'symbol-name tags ", "))
           (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
           (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
           (type (elfeed-entry-content-type elfeed-show-entry))
           (feed (elfeed-entry-feed elfeed-show-entry))
           (feed-title (elfeed-feed-title feed))
           (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
      (erase-buffer)
      (insert "\n")
      (insert (format "%s\n\n" (propertize title 'face 'elfeed-show-title-face)))
      (insert (format "%s\t" (propertize feed-title 'face 'elfeed-search-feed-face)))
      (when (and author elfeed-show-entry-author)
        (insert (format "%s\n" (propertize author 'face 'elfeed-show-author-face))))
      (insert (format "%s\n\n" (propertize nicedate 'face 'elfeed-log-date-face)))
      (when tags
        (insert (format "%s\n"
                        (propertize tagsstr 'face 'elfeed-search-tag-face))))
      ;; (insert (propertize "Link: " 'face 'message-header-name))
      ;; (elfeed-insert-link link link)
      ;; (insert "\n")
      (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
               do (insert (propertize "Enclosure: " 'face 'message-header-name))
               do (elfeed-insert-link (car enclosure))
               do (insert "\n"))
      (insert "\n")
      (if content
          (if (eq type 'html)
              (elfeed-insert-html content base)
            (insert content))
        (insert (propertize "(empty)\n" 'face 'italic)))
      (goto-char (point-min))))

  )
;; 视图效果:1 ends here

;; [[file:config.org::*功能性][功能性:1]]
(after! elfeed-show
  (require 'url)

  (defvar elfeed-pdf-dir
    (expand-file-name "pdfs/"
                      (file-name-directory (directory-file-name elfeed-enclosure-default-dir))))

  (defvar elfeed-link-pdfs
    '(("https://www.jstatsoft.org/index.php/jss/article/view/v0\\([^/]+\\)" . "https://www.jstatsoft.org/index.php/jss/article/view/v0\\1/v\\1.pdf")
      ("http://arxiv.org/abs/\\([^/]+\\)" . "https://arxiv.org/pdf/\\1.pdf"))
    "List of alists of the form (REGEX-FOR-LINK . FORM-FOR-PDF)")

  (defun elfeed-show-pdf (entry)
    (interactive
     (list (or elfeed-show-entry (elfeed-search-selected :ignore-region))))
    (let ((link (elfeed-entry-link entry))
          (feed-name (plist-get (elfeed-feed-meta (elfeed-entry-feed entry)) :title))
          (title (elfeed-entry-title entry))
          (file-view-function
           (lambda (f)
             (when elfeed-show-entry
               (elfeed-kill-buffer))
             (pop-to-buffer (find-file-noselect f))))
          pdf)

      (let ((file (expand-file-name
                   (concat (subst-char-in-string ?/ ?, title) ".pdf")
                   (expand-file-name (subst-char-in-string ?/ ?, feed-name)
                                     elfeed-pdf-dir))))
        (if (file-exists-p file)
            (funcall file-view-function file)
          (dolist (link-pdf elfeed-link-pdfs)
            (when (and (string-match-p (car link-pdf) link)
                       (not pdf))
              (setq pdf (replace-regexp-in-string (car link-pdf) (cdr link-pdf) link))))
          (if (not pdf)
              (message "No associated PDF for entry")
            (message "Fetching %s" pdf)
            (unless (file-exists-p (file-name-directory file))
              (make-directory (file-name-directory file) t))
            (url-copy-file pdf file)
            (funcall file-view-function file))))))
  )
;; 功能性:1 ends here

(defadvice! shut-up-org-problematic-hooks (orig-fn &rest args)
  :around #'org-fancy-priorities-mode
  :around #'org-superstar-mode
  (ignore-errors (apply orig-fn args)))

(cl-defmacro lsp-org-babel-enable (lang)
"Support LANG in org source code block."
(setq centaur-lsp 'lsp-mode)
(cl-check-type lang stringp)
(let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
        (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
    (defun ,intern-pre (info)
        (let ((file-name (->> info caddr (alist-get :file))))
        (unless file-name
            (setq file-name (make-temp-file "babel-lsp-")))
        (setq buffer-file-name file-name)
        (lsp-deferred)))
    (put ',intern-pre 'function-documentation
            (format "Enable lsp-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
    (if (fboundp ',edit-pre)
        (advice-add ',edit-pre :after ',intern-pre)
        (progn
        (defun ,edit-pre (info)
            (,intern-pre info))
        (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))
(defvar org-babel-lang-list
'("go" "python" "ipython" "bash" "sh" "js" "typescript" "css"))
(dolist (lang org-babel-lang-list)
(eval `(lsp-org-babel-enable ,lang)))

(add-hook 'org-mode-hook #'+org-pretty-mode)
(setq org-directory "~/.gclrc/org/"
    org-log-done 'time                        ; having the time a item is done sounds convenient
    org-list-allow-alphabetical t             ; have a. A. a) A) list bullets
    org-export-in-background t                ; run export processes in external emacs process
    org-catch-invisible-edits 'smart          ; try not to accidently do weird stuff in invisible regions
    org-fontify-done-headline t               ; 已完成的加上删除线
    )


(map! :map evil-org-mode-map
    :after evil-org
    :n "g <up>" #'org-backward-heading-same-level
    :n "g <down>" #'org-forward-heading-same-level
    :n "g <left>" #'org-up-element
    :n "g <right>" #'org-down-element)


(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

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

(setq org-roam-directory "~/.doom.d/.local/roam/")
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

(use-package! org-super-agenda
  :commands (org-super-agenda-mode))
(after! org-agenda
  (org-super-agenda-mode))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t
      org-agenda-breadcrumbs-separator " ❱ "
      org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
      )

(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                           :todo "NEXT"
                           :order 1)
                          (:name "Important"
                           :tag "Important"
                           :priority "A"
                           :order 6)
                          (:name "Due Today"
                           :deadline today
                           :order 2)
                          (:name "Due Soon"
                           :deadline future
                           :order 8)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 7)
                          (:name "Assignments"
                           :tag "Assignment"
                           :order 10)
                          (:name "Issues"
                           :tag "Issue"
                           :order 12)
                          (:name "Emacs"
                           :tag "Emacs"
                           :order 13)
                          (:name "Projects"
                           :tag "Project"
                           :order 14)
                          (:name "Research"
                           :tag "Research"
                           :order 15)
                          (:name "To read"
                           :tag "Read"
                           :order 30)
                          (:name "Waiting"
                           :todo "WAITING"
                           :order 20)
                          (:name "University"
                           :tag "uni"
                           :order 32)
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant")
                           :todo ("SOMEDAY" )
                           :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))

(defun org-capture-select-template-prettier (&optional keys)
  "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
         (or (org-contextualize-keys
              (org-capture-upgrade-templates org-capture-templates)
              org-capture-templates-contexts)
             '(("t" "Task" entry (file+headline "" "Tasks")
                "* TODO %?\n  %u\n  %a")))))
    (if keys
        (or (assoc keys org-capture-templates)
            (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
               "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
               "Template key: "
               `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
(advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

(defun org-mks-pretty (table title &optional prompt specials)
  "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
  (save-window-excursion
    (let ((inhibit-quit t)
          (buffer (org-switch-to-buffer-other-window "*Org Select*"))
          (prompt (or prompt "Select: "))
          case-fold-search
          current)
      (unwind-protect
          (catch 'exit
            (while t
              (setq-local evil-normal-state-cursor (list nil))
              (erase-buffer)
              (insert title "\n\n")
              (let ((des-keys nil)
                    (allowed-keys '("\C-g"))
                    (tab-alternatives '("\s" "\t" "\r"))
                    (cursor-type nil))
                ;; Populate allowed keys and descriptions keys
                ;; available with CURRENT selector.
                (let ((re (format "\\`%s\\(.\\)\\'"
                                  (if current (regexp-quote current) "")))
                      (prefix (if current (concat current " ") "")))
                  (dolist (entry table)
                    (pcase entry
                      ;; Description.
                      (`(,(and key (pred (string-match re))) ,desc)
                       (let ((k (match-string 1 key)))
                         (push k des-keys)
                         ;; Keys ending in tab, space or RET are equivalent.
                         (if (member k tab-alternatives)
                             (push "\t" allowed-keys)
                           (push k allowed-keys))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                      ;; Usable entry.
                      (`(,(and key (pred (string-match re))) ,desc . ,_)
                       (let ((k (match-string 1 key)))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                         (push k allowed-keys)))
                      (_ nil))))
                ;; Insert special entries, if any.
                (when specials
                  (insert "─────────────────────────\n")
                  (pcase-dolist (`(,key ,description) specials)
                    (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                    (push key allowed-keys)))
                ;; Display UI and let user select an entry or
                ;; a sub-level prefix.
                (goto-char (point-min))
                (unless (pos-visible-in-window-p (point-max))
                  (org-fit-window-to-buffer))
                (let ((pressed (org--mks-read-key allowed-keys
                                                  prompt
                                                  (not (pos-visible-in-window-p (1- (point-max)))))))
                  (setq current (concat current pressed))
                  (cond
                   ((equal pressed "\C-g") (user-error "Abort"))
                   ;; Selection is a prefix: open a new menu.
                   ((member pressed des-keys))
                   ;; Selection matches an association: return it.
                   ((let ((entry (assoc current table)))
                      (and entry (throw 'exit entry))))
                   ;; Selection matches a special entry: return the
                   ;; selection prefix.
                   ((assoc current specials) (throw 'exit current))
                   (t (error "No entry available")))))))
        (when buffer (kill-buffer buffer))))))
(advice-add 'org-mks :override #'org-mks-pretty)

(use-package! doct
  :commands (doct))

(after! org-capture

  (defun +doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
    (let ((name (pop declaration))
          (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
          (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
          (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
      (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (defun +doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
      (setq doct-templates (mapcar (lambda (template)
                                     (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                 (spec (plist-get (plist-get props :doct) :icon)))
                                       (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                      "\t"
                                                                      (nth 1 template))))
                                     template)
                                   templates))))

  (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

  (defvar +org-capture-recipies  "~/.gclrc/org/recipies.org")

  (defun set-org-capture-templates ()
    (setq org-capture-templates
          (doct `(("Personal todo" :keys "t"
                   :icon ("checklist" :set "octicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a")
                   )
                  ("Personal note" :keys "n"
                   :icon ("sticky-note-o" :set "faicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* %?"
                              "%i %a"))
                  ("Email" :keys "e"
                   :icon ("envelope" :set "faicon" :color "blue")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                              "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                              "about %^{topic}"
                              "%U %i %a"))
                  ("Interesting" :keys "i"
                   :icon ("eye" :set "faicon" :color "lcyan")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Interesting"
                   :type entry
                   :template ("* [ ] %{desc}%? :%{i-type}:"
                              "%i %a")
                   :children (("Webpage" :keys "w"
                               :icon ("globe" :set "faicon" :color "green")
                               :desc "%(org-cliplink-capture) "
                               :i-type "read:web"
                               )
                              ("Article" :keys "a"
                               :icon ("file-text" :set "octicon" :color "yellow")
                               :desc ""
                               :i-type "read:reaserch"
                               )
                              ("\tRecipie" :keys "r"
                               :icon ("spoon" :set "faicon" :color "dorange")
                               :file +org-capture-recipies
                               :headline "Unsorted"
                               :template "%(org-chef-get-recipe-from-url)"
                               )
                              ("Information" :keys "i"
                               :icon ("info-circle" :set "faicon" :color "blue")
                               :desc ""
                               :i-type "read:info"
                               )
                              ("Idea" :keys "I"
                               :icon ("bubble_chart" :set "material" :color "silver")
                               :desc ""
                               :i-type "idea"
                               )))
                  ("Tasks" :keys "k"
                   :icon ("inbox" :set "octicon" :color "yellow")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Tasks"
                   :type entry
                   :template ("* TODO %? %^G%{extra}"
                              "%i %a")
                   :children (("General Task" :keys "k"
                               :icon ("inbox" :set "octicon" :color "yellow")
                               :extra ""
                               )
                              ("Task with deadline" :keys "d"
                               :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                               :extra "\nDEADLINE: %^{Deadline:}t"
                               )
                              ("Scheduled Task" :keys "s"
                               :icon ("calendar" :set "octicon" :color "orange")
                               :extra "\nSCHEDULED: %^{Start time:}t"
                               )
                              ))
                  ("Project" :keys "p"
                   :icon ("repo" :set "octicon" :color "silver")
                   :prepend t
                   :type entry
                   :headline "Inbox"
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :file ""
                   :custom (:time-or-todo "")
                   :children (("Project-local todo" :keys "t"
                               :icon ("checklist" :set "octicon" :color "green")
                               :time-or-todo "TODO"
                               :file +org-capture-project-todo-file)
                              ("Project-local note" :keys "n"
                               :icon ("sticky-note" :set "faicon" :color "yellow")
                               :time-or-todo "%U"
                               :file +org-capture-project-notes-file)
                              ("Project-local changelog" :keys "c"
                               :icon ("list" :set "faicon" :color "blue")
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-project-changelog-file))
                   )
                  ("\tCentralised project templates"
                   :keys "o"
                   :type entry
                   :prepend t
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :children (("Project todo"
                               :keys "t"
                               :prepend nil
                               :time-or-todo "TODO"
                               :heading "Tasks"
                               :file +org-capture-central-project-todo-file)
                              ("Project note"
                               :keys "n"
                               :time-or-todo "%U"
                               :heading "Notes"
                               :file +org-capture-central-project-notes-file)
                              ("Project changelog"
                               :keys "c"
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-central-project-changelog-file))
                   )))))

  (set-org-capture-templates)
  (unless (display-graphic-p)
    (add-hook 'server-after-make-frame-hook
              (defun org-capture-reinitialise-hook ()
                (when (display-graphic-p)
                  (set-org-capture-templates)
                  (remove-hook 'server-after-make-frame-hook
                               #'org-capture-reinitialise-hook))))))

(setf (alist-get 'height +org-capture-frame-parameters) 15)
;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
(setq +org-capture-fn
      (lambda ()
        (interactive)
        (set-window-parameter nil 'mode-line-format 'none)
        (org-capture)))

(use-package! org-pretty-table
  :commands (org-pretty-table-mode global-org-pretty-table-mode))

(use-package! org-chef
  :commands (org-chef-insert-recipe org-chef-get-recipe-from-url))
