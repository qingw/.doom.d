;;; dev/js/config.el -*- lexical-binding: t; -*-

(after! (:any js2-mode rjsx-mode web-mode vue-mode))

(use-package! js-doc
  :config
  (setq js-doc-mail-address user-mail-address
        js-doc-author (format "%s <%s>" (user-full-name) js-doc-mail-address)
        js-doc-url user-blog-url
        js-doc-license "MIT"))

(use-package! prettier-js
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))

(use-package! js2-mode
  :config
  (setq js2-basic-offset 2
        js-switch-indent-offset 2
        js-indent-level 2))

(use-package! web-mode
  :config
  (setq web-mode-attr-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-indent-style 2
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2))
