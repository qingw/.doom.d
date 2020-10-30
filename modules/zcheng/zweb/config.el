;;; dev/js/config.el -*- lexical-binding: t; -*-

;; (after! (:any js2-mode rjsx-mode web-mode vue-mode))

(setq flycheck-javascript-eslint-executable "/usr/local/bin/eslint")

(use-package! css-mode
  :init (setq css-indent-offset 2))

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

(use-package! js2-mode
  :defines flycheck-javascript-eslint-executable
  :init
  :config
  (setq js2-basic-offset 2
        js-switch-indent-offset 2
        js-indent-level 2
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))

(use-package! web-mode
  :mode ("\\.html\\'" "\\.vue\\'")
  :init
  (add-hook 'web-mode-hook #'lsp!)
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  :config
  (setq web-mode-attr-indent-offset 2
        web-mode-code-indent-offset 2
        ;; web-mode-indent-style 2
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-enable-current-element-highlight t
        )
  (setq web-mode-content-types-alist
        '(("vue".  "\\.vue\'")))
  ;; (add-hook 'web-mode-hook 'lsp-vue-enable)
  (add-hook 'web-mode-hook (lambda()
                             (cond ((equal web-mode-content-type "html")
                                    (my/web-html-setup))
                                   ((member web-mode-content-type '("vue"))
                                    (my/web-vue-setup))))))

(use-package! typescript-mode
  :config
  (setq-default typescript-indent-level 2))

;; (use-package! company-lsp
;;   :config
;;   (setq company-lsp-enable-snippet t))

;; (use-package! company
;;   :config
;;   (add-hook 'company-mode-hook 'company-quickhelp-mode)
;;   (add-to-list 'company-backends 'company-lsp))

(use-package! instant-rename-tag)

(add-hook 'prog-mode-hook #'add-node-modules-path)
