;;; zcheng/vue/config.el -*- lexical-binding: t; -*-

(use-package! vue-mode
  :init
  (add-hook 'vue-mode-hook #'lsp!))

(use-package! web-mode
  :init
  (define-derived-mode vue-mode web-mode "Vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  (add-hook 'vue-mode-hook #'spacemacs//vue-setup-editor-style)
  (add-hook 'vue-mode-hook #'spacemacs//vue-setup-keybindings)
  (add-hook 'vue-mode-local-vars-hook #'spacemacs//vue-setup-backend))


(use-package! add-node-modules-path
  :init
  (add-hook 'vue-mode-hook #'add-node-modules-path))


(use-package! company
  :init
  (add-hook 'vue-mode-local-vars-hook #'spacemacs//vue-setup-company))


(use-package! emmet-mode
  :init
  (add-hook 'vue-mode-hook #'emmet-mode))

(use-package! evil-matchit
  :init
  (evilmi-load-plugin-rules '(vue-mode) '(template simple html))
  (add-hook 'vue-mode-hook 'turn-on-evil-matchit-mode))

;;(use-package! flycheck
  ;;:init
  ;;(flycheck-add-mode 'javascript-eslint 'vue-mode)
  ;;)

(use-package! smartparens
  :init
  ;; (add-hook 'vue-mode-hook #'smartparens-strict-mode)
  (add-hook 'vue-mode-hook #'smartparens-mode))

(use-package! yasnippet
  :init
  (add-hook 'vue-mode-hook #'spacemacs//vue-setup-yasnippet))
