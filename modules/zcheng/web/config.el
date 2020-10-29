;;; dev/js/config.el -*- lexical-binding: t; -*-

;; (after! (:any js2-mode rjsx-mode web-mode vue-mode))

(use-package! css-mode
  :init (setq css-indent-offset 2))

(use-package! js-doc
  :config
  (setq js-doc-mail-address user-mail-address
        js-doc-author (format "%s <%s>" (user-full-name) js-doc-mail-address)
        ;; js-doc-url user-blog-url
        js-doc-license "MIT"))

(use-package! prettier-js
  :hook ((js-mode
          js2-mode
          json-mode
          web-mode
          css-mode
          sgml-mode
          html-mode
          vue-mode
          rjsx-mode
          typescript-mode
          )))

(use-package! js2-mode
  :defines flycheck-javascript-eslint-executable
  :init
  :config
  (setq js2-basic-offset 2
        js-switch-indent-offset 2
        js-indent-level 2))

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
  (add-hook 'web-mode-hook (lambda()
                             (cond ((equal web-mode-content-type "html")
                                    (my/web-html-setup))
                                   ((member web-mode-content-type '("vue"))
                                    (my/web-vue-setup)))))
  )
