;; -*- no-byte-compile: t; -*-

;; (when IS-LINUX
;;   (package! eaf :recipe (:host github
;;                             :repo "manateelazycat/emacs-application-framework"
;;                             :files ("*")
;;                             :no-byte-compile t))

(package! move-text)
(package! parrot)
;; fast, friendly searching with ripgrep and Emacs
(package! deadgrep)
(package! ranger)
(package! youdao-dictionary)
;; http://www.baidu.com
(package! link-hint)
(package! deft)
(package! anzu)
(package! pangu-spacing)
(package! visual-regexp)
(package! visual-regexp-steriods
  :recipe (:host github :repo "benma/visual-regexp-steroids.el"))
(package! osx-lib)
(package! crux)
(package! string-inflection)
(package! cnfonts)
(package! valign)
(package! dotenv-mode)

;; window
(package! golden-ratio)
;; (package! zoom)

;; type sound
(package! selectric-mode :pin "1840de71f7414b7cd6ce425747c8e26a413233aa")

(package! org-fancy-priorities)
(package! org-pretty-tags)
(package! org-pretty-table
  :recipe (:host github :repo "Fuco1/org-pretty-table")
  :pin "87772a9469d91770f87bfa788580fca69b9e697a")
;; ~=/* 符号显示优化
(package! org-appear :recipe (:host github :repo "awth13/org-appear")
  :pin "6ee49875f8bdefafbde849f5628d673e9740cf8c")
;; 算术符号显示，如 x 的平方 $a^2$ -> 对应数学表示型式
(package! org-fragtog :pin "0151cabc7aa9f244f82e682b87713b344d780c23")
;; 目录树
(package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree")
  :pin "207c748aa5fea8626be619e8c55bdb1c16118c25")
(package! engrave-faces :recipe (:host github :repo "tecosaur/engrave-faces"))
(package! ox-gfm :pin "99f93011b069e02b37c9660b8fcb45dab086a07f")
(package! org-pandoc-import :recipe
  (:host github :repo "tecosaur/org-pandoc-import" :files ("*.el" "filters" "preprocessors")))
(package! org-roam-server :pin "2122a61e9e9be205355c7e2c1e4b65986d6985a5" :disable t)
(package! org-roam :disable t)

;; (package! rime)
;; (package! liberime)
;; (package! pyim)
;; (package! fcitx)
;; (package! ace-pinyin)
;; (package! posframe :recipe (:host github :repo "tumashu/posframe"))

(package! leetcode)
(package! instant-rename-tag
  :recipe (:host github :repo "manateelazycat/instant-rename-tag"))
(package! js-doc)
(package! imenu-list)
(package! yasnippet-snippets)
(package! git-gutter)

;; web
(package! web-beautify)
(package! prettier-js)
(package! ob-typescript)
(package! phpactor)

;; ast
(package! tree-sitter :disable t)
(package! tree-sitter-langs :disable t)
(package! import-js :disable t)
(package! tide :disable t)
(package! eldoc :disable t)
(package! vue-mode :disable t)
