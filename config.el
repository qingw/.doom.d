;;; config.el -*- lexical-binding: t; -*-

;; 解绑一些按键，待用
(map! "C-s" nil
      "C-d" nil
      "M-," nil
      "M-." nil

      :leader
      "A" nil
      "X" nil)

;; 全局按键绑定
(map! :ni       "C-="           #'er/expand-region
      :ni       "C-e"           #'evil-end-of-line
      :ni       "C-d"           (cmd! (previous-line)
                                      (kill-line)
                                      (forward-line))
      :ni       "C-s r"         #'instant-rename-tag

      :ni       "M-i"           #'parrot-rotate-next-word-at-point

      :ni       "s-<"           #'move-text-up
      :ni       "s->"           #'move-text-down

      :leader
      :n        "SPC"   #'execute-extended-command
      )

;; 个人信息配置
(setq user-full-name "Zhicheng Lee"
      user-mail-address "gccll.love@gmail.com"
      user-blog-url "https://www.cheng92.com")

;; setq, set-default 统一配置的地方
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq which-key-idle-delay 0.5)
(setq org-directory "~/github/documents/org")
(setq display-line-numbers-type t)

;; 主题配置
(setq doom-theme 'doom-one)

(setq doom-font (font-spec :family "JetBrains Mono" :size 24)
      doom-big-font (font-spec :family "JetBrains Mono" :size 36)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 24)
      doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))

;; 全局开启一些模式
(abbrev-mode 1)

;; ------------------- 缩写表 ---------------------------------------------
(define-abbrev-table 'global-abbrev-table '(
                                            ("8imark" "import { marker } from '@commons/sunlight/marker'")
                                            ("8ilib" "import { isArray } from '@commons/sunlight/lib'")
                                            ("81com" "@import '~@commons/styles/common';")
                                            ))

(use-package! parrot
  :config
  (parrot-mode))

(setq parrot-rotate-dict
      '(
        (:rot ("alpha" "beta") :caps t :lower nil)
        ;; => rotations are "Alpha" "Beta"

        (:rot ("snek" "snake" "stawp"))
        ;; => rotations are "snek" "snake" "stawp"

        (:rot ("yes" "no") :caps t :upcase t)
        ;; => rotations are "yes" "no", "Yes" "No", "YES" "NO"

        (:rot ("&" "|"))
        ;; => rotations are "&" "|"
        ;; default dictionary starts here ('v')
        (:rot ("begin" "end") :caps t :upcase t)
        (:rot ("enable" "disable") :caps t :upcase t)
        (:rot ("enter" "exit") :caps t :upcase t)
        (:rot ("forward" "backward") :caps t :upcase t)
        (:rot ("front" "rear" "back") :caps t :upcase t)
        (:rot ("get" "set") :caps t :upcase t)
        (:rot ("high" "low") :caps t :upcase t)
        (:rot ("in" "out") :caps t :upcase t)
        (:rot ("left" "right") :caps t :upcase t)
        (:rot ("min" "max") :caps t :upcase t)
        (:rot ("on" "off") :caps t :upcase t)
        (:rot ("prev" "next"))
        (:rot ("start" "stop") :caps t :upcase t)
        (:rot ("true" "false") :caps t :upcase t)
        (:rot ("&&" "||"))
        (:rot ("==" "!="))
        (:rot ("===" "!=="))
        (:rot ("." "->"))
        (:rot ("if" "else" "elif"))
        (:rot ("ifdef" "ifndef"))
        ;; javascript
        (:rot ("var" "let" "const"))
        (:rot ("null" "undefined"))
        (:rot ("number" "object" "string" "symbol"))

        ;; c/...
        (:rot ("int8_t" "int16_t" "int32_t" "int64_t"))
        (:rot ("uint8_t" "uint16_t" "uint32_t" "uint64_t"))
        (:rot ("1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))
        (:rot ("1st" "2nd" "3rd" "4th" "5th" "6th" "7th" "8th" "9th" "10th"))
        ))

;; web 开发配置
(setq css-indent-offset 2
      js2-basic-offset 2
      js-switch-indent-offset 2
      js-indent-level 2
      js2-mode-show-parse-errors nil
      js2-mode-show-strict-warnings nil
      web-mode-attr-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-markup-indent-offset 2
      web-mode-enable-current-element-highlight t
      web-mode-enable-current-column-highlight t)
(setq-default typescript-indent-level 2)
