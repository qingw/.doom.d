;;; zcheng/vue/config.el -*- lexical-binding: t; -*-

(use-package! vue-mode
  :init
  (add-hook 'vue-mode-hook #'lsp!))
