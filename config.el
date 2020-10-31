;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+bindings")
(load! "+editor")
(load! "+hydra")
(load! "+abbrev")

(setq user-full-name "Zhicheng Lee"
      user-mail-address "gccll.love@gmail.com"
      user-blog-url "https://www.cheng92.com")

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'semi-light))
;; (setq doom-font (font-spec :family "monospace" :size 13 :weight 'semi-light))
 ;;     doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-theme 'doom-one)
(setq org-directory "~/org/")
(setq display-line-numbers-type t)

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

;; (use-package! smart-hungry-delete
;;   :bind (("<backspace>" . smart-hungry-delete-backward-char)
;;          ("C-d" . smart-hungry-delete-forward-char))
;;   :defer nil
;;   :config (smart-hungry-delete-add-default-hooks))

(use-package! parrot
  :config
  (parrot-mode))

(use-package! delsel
  :hook (after-init . delete-selection-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               for web                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq flycheck-javascript-eslint-executable "/usr/local/bin/eslint")
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
      web-mode-enable-current-element-highlight t)
( setq-default typescript-indent-level 2 )


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
  (add-hook 'web-mode-hook 'lsp-vue-enable)
  (add-hook 'web-mode-hook (lambda()
                             (cond ((equal web-mode-content-type "html")
                                    (my/web-html-setup))
                                   ((member web-mode-content-type '("vue"))
                                    (my/web-vue-setup)))))
  )

(use-package! instant-rename-tag)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               for prog                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'prog-mode-hook #'add-node-modules-path)

;; =========================
(add-load-path! "lisp")
