;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el


(package! visual-regexp)
(package! visual-regexp-steriods
  :recipe (:host github :repo "benma/visual-regexp-steroids.el"))
(package! move-text)
(package! ace-window)
(package! crux)
(package! pangu-spacing)
(package! cnfonts)
(package! string-inflection)
(package! osx-lib)
(package! delsel)
(package! parrot)
(package! dash-at-point)

;; web development
(package! prettier-js)
(package! js-doc)
;; (package! lsp-vue)
(package! instant-rename-tag
  :recipe (:host github :repo "manateelazycat/instant-rename-tag"))
;; more development
(package! leetcode)
(package! lsp-ivy)
(package! org-mac-link
  :recipe (:host github :repo "gcclll/org-mac-link"))
(package! ebuku)
