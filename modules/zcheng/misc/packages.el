;; -*- no-byte-compile: t; -*-
;;; zcheng/misc/packages.el


(package! evil)
(package! visual-regexp)
(package! visual-regexp-steriods
  :recipe (:host github :repo "benma/visual-regexp-steroids.el"))
(package! ranger)
(package! move-text)
(package! ace-window)
(package! sidebar
  :recipe (:host github :repo "sebastiencs/sidebar.el") :disable t)
(package! treemacs)
(package! smart-hungry-delete)
(package! crux)
(package! pangu-spacing)
(package! multiple-cursors)
