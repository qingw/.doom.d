;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+bindings")
(load! "+editor")
(load! "+hydra")
(load! "+abbrev")

(setq user-full-name "Zhicheng Lee"
      user-mail-address "gccll.love@gmail.com"
      user-blog-url "https://www.cheng92.com")
(setq which-key-idle-delay 0.5)
;; (setq gc-cons-threshold 100000000)
;; (cnfonts-increase-fontsize)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'semi-light))
;; (setq doom-font (font-spec :family "monospace" :size 13 :weight 'semi-light))
 ;;     doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-theme 'doom-one)
(setq org-directory "~/github/documents/org")
(setq display-line-numbers-type t)
(delete-selection-mode 1)

;; hook
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               for tools                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! visual-regexp
  :commands (vr/select-replace vr/select-query-replace))

(use-package! visual-regexp-steriods
  :commands (vr/select-replace vr/select-query-replace))

(use-package! parrot
  :config
  (parrot-mode))

(use-package! delsel
  :hook (after-init . delete-selection-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               for lsp                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `company'
(after! company
  :config
  (setq
   company-idle-delay 0 ; default 0.2
   company-minimum-prefix-length 2 ; 敲一个字符弹出补全框
   company-show-numbers t ;不显示左侧的数字
   company-require-match nil) ; 允许输入不匹配的字符
  (global-company-mode)

  ;; backends
  (setq
   company-backends (delete 'company-xcode company-backends)
   company-backends (delete 'company-bbdb company-backends)
   company-backends (delete 'company-eclim company-backends)
   company-backends (delete 'company-gtags company-backends)
   company-backends (delete 'company-etags company-backends)
   ;; company-backends (delete 'company-box company-backends)
   company-backends (delete 'company-oddmuse company-backends)
   )

  (add-to-list 'company-backends 'company-files)
  ;; 用 `TAB' 选择且同时补全
  ;; (company-tng-configure-default)
  (setq company-frontends
        '(
          ;; company-tng-frontend
          company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))

  ;; 不小写化返回的候选
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case t)
  )

(use-package! lsp-mode
  :hook (
         (web-mode . lsp)
         (typescript-mode . lsp)
         (rjsx-mode . lsp)
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

(use-package! lsp-ivy :commands lsp-ivy-workspace-symbol)

;; `python'
(use-package! lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

;; https://github.com/emacs-lsp/lsp-mode/wiki/LSP-ESlint-integration
;; (setq lsp-eslint-server-command
;;       '("node"
;;         "/Users/simon/.vscode/extensions/dbaeumer.vscode-eslint-2.1.13/server/out/eslintServer.js"
;;         "--stdio"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               for web: http://web-mode.org/                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;; (setq flycheck-javascript-eslint-executable "/usr/local/bin/eslint")
;; Indent
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
( setq-default typescript-indent-level 2 )

(use-package! rjsx-mode)

(use-package! js-doc
  :config
  (setq js-doc-mail-address user-mail-address
        js-doc-author (format "%s <%s>" (user-full-name) js-doc-mail-address)
        ;; js-doc-url user-blog-url
        js-doc-license "MIT"))

(use-package! prettier-js
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'json-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'css-mode-hook 'prettier-js-mode)
  (add-hook 'html-mode-hook 'prettier-js-mode)
  (add-hook 'vue-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  )

(use-package! web-mode
  :mode ("\\.html\\'" "\\.vue\\'")
  :init
  (add-hook 'web-mode-hook #'lsp!)
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  :config
  (setq web-mode-content-type-alist
        '(("vue" . "\\.vue\'")))
  ;; (add-hook 'web-mode-hook 'lsp-vue-enable)
  (add-hook 'web-mode-hook (lambda()
                             (cond ((equal web-mode-content-type "html")
                                    (my/web-html-setup))
                                   ((member web-mode-content-type '("vue"))
                                    (my/web-vue-setup)))))
  )

(use-package! instant-rename-tag)

;; (use-package! lsp-vue)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               for prog                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook #'add-node-modules-path)

(after! leetcode
  (setq leetcode-prefer-language "javascript"
        leetcode-prefer-sql "mysql"
        leetcode-save-solutions t
        leetcode-directory "~/github/make-leetcode"))

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))
;; =========================
(add-load-path! "lisp")
