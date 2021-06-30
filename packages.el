;; -*- no-byte-compile: t; -*-

(package! smart-hungry-delete)
(package! move-text)
(package! parrot)
;; fast, friendly searching with ripgrep and Emacs
(package! deadgrep)
(package! ranger)
(package! youdao-dictionary)
(package! link-hint)
(package! deft)
(package! anzu)
(package! visual-regexp)
(package! visual-regexp-steriods
  :recipe (:host github :repo "benma/visual-regexp-steroids.el"))
(package! osx-lib)
(package! crux)
(package! string-inflection)
(package! pangu-spacing)
(package! cnfonts)
(package! valign)

;; org
(package! org-fancy-priorities)
(package! org-pretty-tags)
(package! org-roam :disable t)
;; (package! org-special-block-extras)

;; development
(package! leetcode)
(package! instant-rename-tag
  :recipe (:host github :repo "manateelazycat/instant-rename-tag"))
(package! js-doc)
(package! imenu-list)
(package! yasnippet-snippets)

;; web dev
;; (package! ob-typescript)
(package! web-beautify)
(package! prettier-js)
(package! import-js :disable t)
(package! tide :disable t)
(package! eldoc :disable t)
;; (package! lsp-css
;;       :recipe (:host
;;                github
;;                :repo "emacs-lsp/lsp-css"))

;; java
(package! lsp-java)
(package! dap-mode)

;; (package! ox-rst
;;   :recipe (:host github :repo "msnoigrs/ox-rst"))
;; (package! scrollkeeper
;;   :recipe (:host github :repo "alphapapa/scrollkeeper.el"))
;; (package! gif-screencast)

;; pyim
(package! liberime
  :recipe (:host github :repo "merrickluo/liberime"
           :files ("CMakeLists.txt" "Makefile" "src" "*.el")))
(package! pyim-greatdict
  :recipe (:host github :repo "tumashu/pyim-greatdict"))
(package! pyim-wbdict
  :recipe (:host github :repo "tumashu/pyim-wbdict"
           :files ("*.el")))
(package! pyim)
