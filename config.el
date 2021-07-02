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

(map! :niv      "C-s" nil
      :niv      "C-d" nil
      :niv      "C-i" nil
      :niv      "M-," nil
      :niv      "M-." nil

      :leader
      "A" nil
      "X" nil
      "/" nil)

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
      "s-)"     #'sp-forward-barf-sexp)

(map!
 "C-'"     #'imenu-list-smart-toggle
 "C-s"     #'+default/search-buffer

 ;; smartparen
 "C-("     #'sp-backward-slurp-sexp
 "C-)"     #'sp-forward-slurp-sexp

 ;; C-c
 "C-c a c"     #'org-mac-chrome-insert-frontmost-url
 "C-c d"       #'insert-current-date-time
 "C-c t"       #'insert-current-time
 "C-c o"       #'crux-open-with
 "C-c r"       #'vr/replace
 "C-c q"       #'vr/query-replace
 "C-c u"       #'crux-view-url
 "C-c y"       #'youdao-dictionary-search-at-point-posframe

 ;; C-c l
 "C-c l o"      #'link-hint-open-link
 "C-c l c"      #'link-hint-copy-link
 "C-c l a"      #'link-hint-open-link-at-point
 "C-c l C"      #'link-hint-copy-link-at-point

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

(defun +chinese*org-html-paragraph (paragraph contents info)
  "Join consecutive Chinese lines into a single long line without unwanted space
when exporting org-mode to html."
  (let* ((fix-regexp "[[:multibyte:]]")
         (origin-contents contents)
         (fixed-contents
          (replace-regexp-in-string
           (concat "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)")
           "\\1\\2"
           origin-contents)))
    (list paragraph fixed-contents info)))
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

(use-package! git-gutter
  :config
  (global-git-gutter-mode 't))

(setq yas-triggers-in-field t)

(use-package! doom-snippets             ; hlissner
  :after yasnippet)

(use-package! yasnippet-snippets        ; AndreaCrotti
  :after yasnippet)

(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))

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
