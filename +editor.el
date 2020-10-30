;;; +editor.el -*- lexical-binding: t; -*-

;; use space instead of tabs
(setq indent-tabs-mode nil)
(setq-default abbrev-mode t
              fill-column 80)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
(global-prettify-symbols-mode t)
(global-pangu-spacing-mode 1)
(global-eldoc-mode -1)
