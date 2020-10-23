;;; zcheng/vue/autoload.el -*- lexical-binding: t; -*-
;;;
;;;###autoload
(defun spacemacs//vue-setup-backend ()
  "Conditionally setup vue backend."
  (pcase vue-backend
    ('dumb (spacemacs//vue-setup-dumb))
    ('lsp (spacemacs//vue-setup-lsp))))

;;;###autoload
(defun spacemacs//vue-setup-company ()
  "Conditionally setup company based on backend."
  (pcase vue-backend
    ('dumb (spacemacs//vue-setup-dumb-company))
    ('lsp (spacemacs//vue-setup-lsp-company))))

;;;###autoload
(defun spacemacs//vue-setup-lsp ()
  "Setup lsp backend."
  ;; (if (configuration-layer/layer-used-p 'lsp)
  ;;     (progn
  ;;       ;; error checking from lsp langserver sucks, turn it off
  ;;       ;; so eslint won't be overriden
  ;;       (setq-local lsp-prefer-flymake :none)
  ;;       (lsp))
  ;;   ;; (flycheck-select-checker 'javascript-eslint))
  ;;   (message (concat "`lsp' layer is not installed, "
  ;;                    "please add `lsp' layer to your dotfile.")))
  )

;;;###autoload
(defun spacemacs//vue-setup-lsp-company ()
  "Setup lsp auto-completion."
  ;; (if (configuration-layer/layer-used-p 'lsp)
  ;;     (progn
  ;;       (spacemacs|add-company-backends
  ;;         :backends company-lsp
  ;;         :modes vue-mode
  ;;         :variables company-minimum-prefix-length 2
  ;;         :append-hooks nil
  ;;         :call-hooks t)
  ;;       (company-mode))
  ;;   (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile."))
  )

;;;###autoload
(defun spacemacs//vue-setup-dumb-imenu ()
  (setq imenu-generic-expression '(("html" "^<template>$" 0)
                                   ("js" "^<script>$" 0)
                                   ("js" "^\\s-*\\(data\\).*()\\s-?{" 1)
                                   ("js" "^\\s-*\\(mounted\\).*()\\s-?{" 1)
                                   ("js" "^\\s-*\\(beforeMount\\).*()\\s-?{" 1)
                                   ("js" "^\\s-*\\(beforeDestroy\\).*()\\s-?{" 1)
                                   ("js" "^\\s-*\\(created\\).*()\\s-?{" 1)
                                   ("js" "^\\s-*\\(computed\\):\\s-?{" 1)
                                   ("js" "^\\s-*\\(watch\\):\\s-?{" 1)
                                   ("js" "^\\s-*\\(methods\\):\\s-?{" 1)
                                   ("js" "^\\s-*\\(props\\):\\s-?{" 1)
                                   ("css" "^<css>$" 0))
        imenu-create-index-function #'imenu-default-create-index-function))

;;;###autoload
(defun spacemacs//vue-setup-dumb ()
  (add-to-list 'spacemacs-jump-handlers-vue-mode 'dumb-jump-go)
  (spacemacs//vue-setup-dumb-imenu))

;;;###autoload
(defun spacemacs//vue-setup-dumb-company ()
  ;; (spacemacs|add-company-backends :backends (company-web-html company-css company-files company-dabbrev)
  ;;                                 :modes vue-mode
  ;;                                 :variables company-minimum-prefix-length 2)
  (company-mode))

;;;###autoload
(defun spacemacs//vue-setup-yasnippet ()
  ;; (spacemacs/load-yasnippet)
  (yas-activate-extra-mode 'js-mode))

;;;###autoload
(defun spacemacs//vue-setup-editor-style ()
  "such as indent rules comment style etc"
  ;; https://stackoverflow.com/questions/36701024/how-can-i-indent-inline-javascript-in-web-mode
  (setq web-mode-script-padding 0)
  ;; https://emacs.stackexchange.com/questions/27683/change-comment-style-in-web-mode
  (add-to-list 'web-mode-comment-formats '("javascript" . "//")))

;;;###autoload
(defun spacemacs//vue-setup-keybindings ()
  "free stuff from `html' layer"
  (map! :leader
        :map vue-mode-map
        "El" #'web-mode-dom-errors-show
        "eb" #'web-mode-element-beginning
        "ec" #'web-mode-element-chid
        "ep" #'web-mode-element-parent
        "es" #'web-mode-element-sibling-next
        ;; "hp" #'web-mode-dom-xpath
        "rc" #'web-mode-element-clone
        "rd" #'web-mode-element-vanish
        "rk" #'web-mode-element-kill
        "rn" #'web-mode-element-rename
        "rw" #'web-mode-element-wrap
        ))
