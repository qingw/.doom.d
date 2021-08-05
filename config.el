;;;###autoload
(defun gcl/edit-zsh-configuration ()
  (interactive)
  (find-file "~/.zshrc"))

;;;###autoload
(defun gcl/use-eslint-from-node-modules ()
    "Set local eslint if available."
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

;;;###autoload
(defun gcl/goto-match-paren (arg)
  "Go to the matching if on (){}[], similar to vi style of % ."
  (interactive "p")
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

;;;###autoload
(defun gcl/string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   (t
    ;; default
    (string-inflection-all-cycle))))

;; Current time and date
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%H:%M"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

;;;###autoload
(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time)))
  )

;;;###autoload
(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time)))
  )

;;;###autoload
(defun my/capitalize-first-char (&optional string)
  "Capitalize only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))

;;;###autoload
(defun my/lowcase-first-char (&optional string)
  "Capitalize only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat first-char rest-str))))

;;;###autoload
(defun gcl/async-shell-command-silently (command)
  "async shell command silently."
  (interactive)
  (let
      ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*.*"
          (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command)))

;;;###autoload
(defun aj-fix-buffer-file-name-for-indirect-buffers-a (orig-fn &rest args)
  "Advice for functions expecting `buffer-file-name' to work."
  (let ((buffer-file-name buffer-file-truename))
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&optional buffer)
                 "Return value of `buffer-file-truename'."
                 (with-current-buffer (or buffer (current-buffer))
                   buffer-file-truename))))
      (apply orig-fn args))))

;;;###autoload
(defun aj-zeal-at-point-run-search-on-wsl-a (search)
  "Launch Windows Zeal from WSL emacs.
Use `call-process' instead of `start-process'.
Use in conjunction with
https://github.com/Konfekt/wsl-gui-bins/blob/master/zeal
"
  (call-process (executable-find "zeal") nil 0 nil search))

;;;###autoload
(defun gcl/indent-org-block-automatically ()
  (interactive)
  (when (org-in-src-block-p)
   (org-edit-special)
   (indent-region (point-min) (point-max))
   (org-edit-src-exit)))

;;;###autoload
(defun aj-org-agenda-save-and-refresh-a (&rest _)
  "Save org files and refresh.
Only org files contributing to `org-agenda' are saved.
Refreshed are `org-agenda' org `org-ql-view', depending on
which one is currently active."
  (org-save-all-org-buffers)
  (if (string-match "Org QL" (buffer-name))
      (org-ql-view-refresh)
    (org-agenda-redo)))

;;;###autoload
(defun aj-org-roam-setup-dailies-file-h ()
  "Setup org-roam dailies file to my taste.
Initialy create id inside top-level \":PROPERTIES:\" drawer.
Finally save buffer.
"
  (let ((fname (or (buffer-file-name)
                   (buffer-file-name (buffer-base-buffer))))
        hstub)
    ;; Run this only when file is newly created (hasn't been saved yet)
    (unless (file-exists-p fname)
      (org-id-get-create)
      (save-buffer))

    (goto-char (point-max))
    (newline)
    ;; prompt for HH:MM if we are not in present day file
    (if (string-equal (format-time-string "%Y-%m-%d")
                      (file-name-sans-extension
                       (file-name-nondirectory
                        (or (buffer-file-name)
                            (buffer-file-name (buffer-base-buffer))))))
        (setq hstub (format-time-string "* %H:%M " (current-time)))
      (setq hstub (concat "* " (ivy-read
                                "Time of the day (HH:MM): "
                                nil)
                          " ")))
    (insert hstub)
    (evil-insert 0)))

;;;###autoload
(defun dired-timesort (filename &optional wildcards)
  (let ((dired-listing-switches "-lhat"))
    (dired filename wildcards)))

;;;###autoload
(defmacro quick-find (key file &optional path find-args)
  `(bind-key
    ,key
    (cond
     ((stringp ,find-args)
      '(lambda (&optional arg)
         (interactive)
         (find-dired (expand-file-name ,file ,path) ,find-args)))
     ((and
       ;; (not (tramp-tramp-file-p (expand-file-name ,file ,path)))
       (or (file-directory-p (expand-file-name ,file ,path))
           (not (file-exists-p (expand-file-name ,file ,path)))))
      '(lambda (&optional arg)
         (interactive)
         (dired-timesort (expand-file-name ,file ,path))))
     (t
      '(lambda (&optional arg)
         (interactive)
         (find-file (expand-file-name ,file ,path)))))))

;;;###autoload
(defun gcl/embrace-prog-mode-hook ()
  (dolist (lst '((?` "`" . "`")))
    (embrace-add-pair (car lst) (cadr lst) (cddr lst))))

;;;###autoload
(defun gcl/embrace-org-mode-hook ()
  (dolist (lst '((?c "@@html:<font color=\"red\">" . "</font>@@")))
    (embrace-add-pair (car lst) (cadr lst) (cddr lst))))

(setq doom-theme 'doom-vibrant)

;; (setq doom-font (font-spec :family "JetBrains Mono" :size 16))
(setq doom-font (font-spec :family "Fira Code" :size 15))
;; (setq doom-font (font-spec :family "Source Code Pro" :size 15))

;; set title
;; (setq frame-title-format
;;       '(""
;;         ;; (:eval
;;         ;;  (if (s-contains-p org-roam-directory (or buffer-file-name ""))
;;         ;;      (replace-regexp-in-string
;;         ;;       ".*/[0-9]*-?" "☰ "
;;         ;;       (subst-char-in-string ?_ ?  buffer-file-name))
;;         ;;    "%b"))
;;         (:eval
;;          (let ((project-name (projectile-project-name)))
;;            (unless (string= "-" project-name)
;;              (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

(doom-load-envvars-file "~/.doom.d/env" )

(defadvice! +literate-tangle-async-h ()
  "A very simplified version of `+literate-tangle-h', but async."
  :override #'+literate-tangle-h
  (let ((default-directory doom-private-dir))
    (gcl/async-shell-command-silently (format "emacs --batch --eval \"(progn \
(require 'org) (setq org-confirm-babel-evaluate nil) \
(org-babel-tangle-file \\\"%s\\\"))\" \
&& /bin/bash ~/.gclrc/bin/rsync-doom-config"
             +literate-config-file))))

;; 启动全屏
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; 个人信息配置
(setq user-full-name "Zhicheng Lee"
      user-mail-address "gccll.love@gmail.com"
      user-blog-url "https://www.cheng92.com"
      read-process-output-max (* 1024 1024)
      display-line-numbers-type t

      ;; web, js, css
      css-indent-offset 2
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
      web-mode-enable-current-column-highlight t

      ;; org
      org-roam-v2-ack t                 ; close v2 warning
      org-roam-directory "~/.gclrc/roam/"
      org-directory "~/.gclrc/org/"
      org-log-done 'time                        ; having the time a item is done sounds convenient
      org-list-allow-alphabetical t             ; have a. A. a) A) list bullets
      org-export-in-background t                ; run export processes in external emacs process
      org-catch-invisible-edits 'smart          ; try not to accidently do weird stuff in invisible regions
      org-fontify-done-headline t               ; 已完成的加上删除线

      ;; scroll behavior
      redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1

      ;; mouse wheel
      mouse-wheel-follow-mouse 't
      mouse-wheel-scroll-amount '(1 ((shift) . 1))

      vc-log-view-type nil
      ;; mu4e
      +mu4e-backend 'offlineimap

      ;; osx
      ;; browse-url-browser-function 'browse-url-default-macosx-browser
      )

(setq-default
 fill-column 80
 undo-limit 80000000
 delete-by-moving-to-trash t
 window-combination-resize t
 delete-trailing-lines t
 x-stretch-cursor t
 typescript-indent-level 2
 custom-file (expand-file-name ".custom.el" doom-private-dir))

(when (file-exists-p custom-file)
  (load custom-file))

(global-set-key (kbd "C-d") nil)        ; ns-print-buffer
(global-set-key (kbd "s-p") nil)        ; ns-print-buffer
(global-set-key (kbd "<f1>") nil)        ; ns-print-buffer
(global-set-key (kbd "<f2>") nil)        ; ns-print-buffer
(define-key evil-normal-state-map (kbd ",") nil)
(define-key evil-visual-state-map (kbd ",") nil)
;; (global-set-key (kbd ",") nil)
(map! "C-e" nil
      :n "C-t" nil)
;; (undefine-key! "SPC :" "SPC ~" "SPC ." "SPC X" "C-c C-r" ",")
;; (undefine-key! evil-normal-state-map ",")
(undefine-key! org-mode-map "C-c C-r")

(global-set-key (kbd "<f1>") 'gcl-everything/body)
(global-set-key (kbd "<f5>") 'deadgrep)
(global-set-key (kbd "<M-f5>") 'deadgrep-kill-all-buffers)
(global-set-key (kbd "<f8>") 'quickrun)
(global-set-key (kbd "<f12>") 'smerge-vc-next-conflict)
(global-set-key (kbd "<f11>") '+vc/smerge-hydra/body)
(global-set-key (kbd "C-t") '+vterm/toggle)
(global-set-key (kbd "C-S-t") '+vterm/here)
(global-set-key (kbd "C-d") 'kill-current-buffer)

(map!
 ;; "M-1"          #'bm-toggle
 ;; "M-2"          #'bm-next
 ;; "M-@"          #'bm-previous
 "M--"          #'gcl/goto-match-paren
 "M-i"          #'parrot-rotate-next-word-at-point
 "M-TAB"        #'gcl-everything/body
 ;; "S-M-SPC"      #'counsel-osx-app
 )

(map!
 :niv   "C-e"   #'evil-end-of-line
 :niv   "C-="   #'er/expand-region

 "C-a"          #'crux-move-beginning-of-line
 "C-s"          #'+default/search-buffer
 )

(map!
 ;; a -> applications, ...
 "C-c a"        #'org-agenda

 ;; b -> bookmark, buffer ...
 "C-c b l"      #'bm-show-all
 "C-c b s"      #'bm-buffer-save

 ;; i -> date, time, ...
 "C-c i d"      #'insert-current-date-time
 "C-c i t"      #'insert-current-time

 ;; f -> file, directory, ...
 "C-c f o"      #'crux-open-with

 ;; h -> help
 "C-c h d"      #'dash-at-point
 "C-c h D"      #'dash-at-point-with-docset

 ;; n -> network utils
 ;; "C-c n x"      #'xxx

 ;; s -> search/engine ...
 ;; "C-c s r"  ;; ...

 ;; s -> replace
 "C-c r r"      #'vr/replace
 "C-c r q"      #'vr/query-replace

 ;; u -> url, ...
 "C-c u u"      #'crux-view-url
 "C-c u o"      #'link-hint-open-link
 "C-c u c"      #'link-hint-copy-link
 "C-c u a"      #'link-hint-open-link-at-point
 "C-c u C"      #'link-hint-copy-link-at-point

 ;; y -> youdao, ...
 "C-c y y"      #'youdao-dictionary-search-at-point+

 "C-c C-r C-r"          #'verb-send-request-on-point-other-window-stay
 "C-c C-r C-s"          #'verb-send-request-on-point-other-window
 "C-c C-r C-f"          #'verb-send-request-on-point
 "C-c C-r C-m"          #'verb-send-request-on-point-no-window
 "C-c C-r C-k"          #'verb-kill-response-buffer-and-window
 "C-c C-r C-a"          #'verb-kill-all-response-buffers
 "C-c C-r C-u"          #'verb-export-request-on-point-curl
 "C-c C-r C-b"          #'verb-export-request-on-point-verb
 "C-c C-r C-w"          #'verb-export-request-on-point-eww
 "C-c C-r C-l"          #'verb-show-vars        ; 查看已存在的变量值列表
 "C-c C-r C-v"          #'verb-set-var

 )

(map!
 ;; "C-x p"        #'vmd-mode
 ;; "C-x d"        #'dash-at-point
 ;; "C-x D"        #'dash-at-point-with-docset
 )

(map!
 "s-<"          #'move-text-up
 "s->"          #'move-text-down
 ;; "s-'"          #'cycle-quotes
 "s-i"          #'gcl/string-inflection-cycle-auto
 ;; projector --- ---
 ;; "s-p b"      #'projector-switch-to-shell-buffer
 ;; "s-p B"      #'projector-run-shell-command-current-directory-background
 ;; "s-p c"      #'projector-run-shell-command-current-directory
 ;; "s-p d"      #'projector-run-default-shell-command
 ;; "s-p r"      #'projector-run-shell-command-project-root
 ;; "s-p R"      #'projector-rerun-buffer-process
 )

(evil-define-minor-mode-key '(normal motion) 'evil-snipe-local-mode
  "s" #'avy-goto-char
  "S" #'avy-goto-char-2
  "w" #'avy-goto-word-1
  "W" #'avy-goto-word-0
  )

(evil-define-key '(normal motion visual) map
   "s" #'avy-goto-char
   "S" #'avy-goto-char-2
   "w" #'avy-goto-word-1
   "W" #'avy-goto-word-0
  )

(map!
 :n     "+"     #'evil-numbers/inc-at-pt
 :n     "-"     #'evil-numbers/dec-at-pt

 ;; g
 :n     "g["    #'beginning-of-defun
 :n     "g]"    #'end-of-defun
 :n     "gd"    #'xref-find-definitions
 :n     "gD"    #'xref-find-references
 :n     "gb"    #'xref-pop-marker-stack

 ;; z
 :n     "z-"    #'sp-splice-sexp
 :n     "z."    #'emmet-wrap-with-markup

 :n     "gi"    nil
 (:prefix ("gi" . "mine")
  :n    "f"     #'+org/attach-file-and-insert-link)

 :nv     ","     nil
 (:prefix ("," . "gccll")
  :nv    "`"     #'gcl-everything/body

  ;; embrace
  (:prefix ("e" . "embrace")
   :n    "a"    #'embrace-add
   :n    "c"    #'embrace-change
   :n    "d"    #'embrace-delete
   )

  )
 )

;; remap gs-> keybinding
(map! :after evil-easymotion
      :map evilem-map
      "c"       #'avy-goto-char
      "C"       #'avy-goto-char-2
      "w"       #'avy-goto-word-1
      "W"       #'avy-goto-word-0
      "ll"      #'avy-goto-line
      "lu"      #'avy-goto-line-above
      "ld"      #'avy-goto-line-below
      )

(map!
 :leader
 :nv    "SPC"           #'execute-extended-command

 (:prefix ("a" . "Applications")
  :n    "e"     #'emms
  :n    "E"     #'emms-smart-browse
  )

 ;; b -> Buffer
 :n     "bf"            #'osx-lib-reveal-in-finder

 ;; f -> File
 :n     "fo"            #'crux-open-with
 :n     "fj"            #'dired-jump

 ;; d -> directory
 :n     "dd"            #'deft

 ;; d -> edit
 :n     "es"            #'sudo-edit

 ;; i -> Insert, Imenu
 :n     "ia"            #'+org/attach-file-and-insert-link
 :n     "im"            #'imenu-list
 :n     "iM"            #'lsp-ui-imenu

 ;; l -> load, ...
 :n     "lr"            #'ranger
 :n     "ld"            #'dired

 ;; r -> Run
 ;; :n     "rp"         #'projector-run-shell-command-project-root
 ;; :n     "rP"         #'projector-run-default-shell-command

 ;; s -> search ?
 (:map (scss-mode-map css-mode-map)
  :n     "si"            #'counsel-css
  )

 ;; / -> Search
 ;; :n     "/r"    #'deadgrep
 )

(map! :map org-mode-map
      (:prefix "t"
       :n "t" #'org-todo
       :n "T" #'counsel-org-tag
       (:prefix ("c" . "checkbox")
        :n "c" #'org-toggle-checkbox
        :n "u" #'org-update-checkbox-count
        )
       (:prefix ("p" . "priority")
        :n "p" #'org-priority
        :n "u" #'org-priority-up
        :n "d" #'org-priority-down
        ))

      (:prefix "C-c"
       (:prefix ( "d" . "Do" )
        "f" #'gcl/indent-org-block-automatically
        )
       (:prefix ("e" . "Emoji")
        "e" #'all-the-icons-insert
        "a" #'all-the-icons-insert-faicon
        "f" #'all-the-icons-insert-fileicon
        "w" #'all-the-icons-insert-wicon
        "o" #'all-the-icons-insert-octicon
        "m" #'all-the-icons-insert-material
        "i" #'all-the-icons-insert-alltheicon
        )
       (:prefix ("c" . "Org Clock")
        "i" #'org-clock-in
        "o" #'org-clock-out
        "h" #'counsel-org-clock-history
        "g" #'counsel-org-clock-goto
        "c" #'counsel-org-clock-context
        "r" #'counsel-org-clock-rebuild-history
        )
       (:prefix ("i" . "Insert")
        "u" #'org-mac-chrome-insert-frontmost-url
        "c" #'copyright
        )

       ;; org-roam, C-c r <x>
       (:prefix ("C-r" . "Verb")
        "C-r"     #'verb-send-request-on-point-other-window-stay
        "C-s"     #'verb-send-request-on-point-other-window
        "C-f"     #'verb-send-request-on-point
        "C-m"     #'verb-send-request-on-point-no-window
        "C-k"     #'verb-kill-response-buffer-and-window
        "C-a"     #'verb-kill-all-response-buffers
        "C-u"     #'verb-export-request-on-point-curl
        "C-b"     #'verb-export-request-on-point-verb
        "C-w"     #'verb-export-request-on-point-eww
        "C-l"     #'verb-show-vars        ; 查看已存在的变量值列表
        )
       )
      )

(quick-find "C-h C-x C-s" "~/.ssh/config")
(quick-find "C-h C-x C-z" "~/.zshrc")
(quick-find "C-h C-x C-d" "~/.gclrc/org/todo.org")
(quick-find "C-h C-x C-o" "~/.offlineimaprc")

(use-package! autoinsert
  :hook
  (find-file . auto-insert))

(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

(after! avy
  ;; home row priorities: 8 6 4 5 - - 1 2 3 7
  (setq avy-keys '(?n ?e ?i ?s ?t ?r ?i ?a)))

(use-package! bm
  :bind
  ("M-1" . bm-toggle)
  ("M-2" . bm-next)
  ("M-@" . bm-previous)
  :custom
  (bm-cycle-all-buffers t)
  :config
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (setq bm-repository-file "~/.doom.d/bm-repository")
  (setq-default bm-buffer-persistence t)
  )

(use-package! color-rg
  :commands (color-rg-search-input
             color-rg-search-symbol
             color-rg-search-input-in-project)
  :bind
  (:map isearch-mode-map
   ("M-s M-s" . isearch-toggle-color-rg)))

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

(use-package! counsel-osx-app
  :bind* ("S-M-SPC" . counsel-osx-app)
  :commands counsel-osx-app
  :config
  (setq counsel-osx-app-location
        (list "/Applications"
              "/Applications/Misc"
              "/Applications/Utilities"
              (expand-file-name "~/Applications")
              (expand-file-name "~/.nix-profile/Applications")
              "/Applications/Xcode.app/Contents/Applications")))

(use-package! cycle-quotes
  :bind
  ("s-'" . cycle-quotes))

;; (use-package! eaf
;;  :commands (eaf-open-browser eaf-open find-file)
;;  :config
;;  (use-package! ctable)
;;  (use-package! deferred)
;;  (use-package! epc))

(use-package! emacs-everywhere
  :if (daemonp)
  :config
  (require 'spell-fu)
  (setq emacs-everywhere-major-mode-function #'org-mode
        emacs-everywhere-frame-name-format "Edit ∷ %s — %s")
  (defadvice! emacs-everywhere-raise-frame ()
    :after #'emacs-everywhere-set-frame-name
    (setq emacs-everywhere-frame-name (format emacs-everywhere-frame-name-format
                                (emacs-everywhere-app-class emacs-everywhere-current-app)
                                (truncate-string-to-width
                                 (emacs-everywhere-app-title emacs-everywhere-current-app)
                                 45 nil nil "…")))
    ;; need to wait till frame refresh happen before really set
    (run-with-timer 0.1 nil #'emacs-everywhere-raise-frame-1))
  (defun emacs-everywhere-raise-frame-1 ()
    (call-process "wmctrl" nil nil nil "-a" emacs-everywhere-frame-name)))

(add-hook 'org-mode-hook 'gcl/embrace-org-mode-hook)
(add-hook 'prog-mode-hook 'gcl/embrace-prog-mode-hook)

(use-package! engine-mode
  :config
  (engine/set-keymap-prefix (kbd "C-c s"))
  ;; (setq engine/browser-function 'eww-browse-url)
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
    :keybinding "a")
  (defengine baidu "https://www.baidu.com/s?wd=%s"
    :keybinding "bb")
  (defengine baidu-image "https://image.baidu.com/search/index?tn=baiduimage&word=%s"
    :keybinding "bi")
  (defengine ctan
    "http://www.ctan.org/search/?x=1&PORTAL=on&phrase=%s"
    :docstring "Search the Comprehensive TeX Archive Network (ctan.org)"
    :keybinding "c")
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")
  (defengine qwant
    "https://www.qwant.com/?q=%s"
    :docstring "什么都能搜到哦~~😍😍"
    :keybinding "q")
  (defengine rfcs
    "http://pretty-rfc.herokuapp.com/search?q=%s"
    :keybinding "r")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")
  (defengine twitter
    "https://twitter.com/search?q=%s"
    :keybinding "t")
  (defengine wolfram-alpha
    "http://www.wolframalpha.com/input/?i=%s"
    :docstring "数学搜索引擎，公式，坐标图等。"
    :keybinding "w") ; 数学搜索引擎，公式，坐标图等
  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
    :keybinding "/i")
  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :docstring "Mappin' it up."
    :keybinding "/m")
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "//")

  (engine-mode 1))

(use-package! dash-at-point
  :bind
  (("C-x d" . dash-at-point)
   ("C-x D" . dash-at-point-with-docset)))

(use-package! delsel
  :config
  (delete-selection-mode t))

(use-package! dotenv-mode
  :mode ("\\.env\\.?.*\\'" . dotenv-mode))

;; (defalias 'ex! 'evil-ex-define-cmd)

;; 快捷操作，通过 : 冒号进入 evil 命令模式
;; File operations
;; (ex! "cp"          #'+evil:copy-this-file)
;; (ex! "mv"          #'+evil:move-this-file)
;; (ex! "rm"          #'+evil:delete-this-file)

;; window 操作
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(use-package! flycheck
    :config
    (add-hook 'after-init-hook 'global-flycheck-mode)
    (add-hook 'flycheck-mode-hook 'gcl/use-eslint-from-node-modules))

(use-package! git-gutter
  :config
  (global-git-gutter-mode 't))

(use-package! hungry-delete
  :config
  (add-hook! 'after-init-hook #'global-hungry-delete-mode)
  (global-hungry-delete-mode 1))

(map! :map dired-mode-map
      "<f2>"    #'gcl-dired/body
      :map ranger-mode-map
      "<f2>"    #'gcl-dired/body
      :map org-agenda-mode-map
      "<f2>"    #'gcl-agenda-view/body
      :map web-mode-map
      "<f2>"    #'hydra-web-mode/body
      )

(defhydra gcl-jump-hydra (:color blue :columns 3 :hint nil)
  "Jump -> Body"
  ("a" gcl-agenda-view/body "Org-Agenda")
  ("c" gcl-jump-char/body "Char Jump")
  ("l" gcl-jump-line/body "Line Jump")
  ("w" gcl-jump-word/body "Word Jump")

  )

(defhydra gcl-repl-hydra (:color blue :columns 3 :hint nil)
  "REPL "
  ("e" ielm " ELisp")
  ("h" httprepl " HTTP")
  ("j" jq-interactivly " JSON")
  ("l" +lua/open-repl " Lua")
  ("n" nodejs-repl " Node.js")
  ("p" +python/open-repl " Python")
  ("s" skewer-repl " Skewer")
  )



(defhydra gcl-everything (:color blue :columns 3 :hint nil)
  "🗯 做任何你想不到的事情~~~~ 👁👁👁👁👁👁👁👁👁
🌻"
  ("j" gcl-jump-hydra/body "Avy")
  ("r" gcl-repl-hydra/body "REPL")
  ("v" gcl-verb-hydra/body "Verb")
  )

(defhydra gcl-jump-char (:color blue :columns 3 :hint nil)
  "Jump By Char ->"
  ("c" avy-goto-char "Char")
  ("l" avy-goto-char-in-line "In line")
  ("t" avy-goto-char-timer "Timer")
  ("2" avy-goto-char-2 "Char2")
  ("a" avy-goto-char-2-above "Above")
  ("b" avy-goto-char-2-below "Below")
  )

(defhydra gcl-jump-line (:color blue :columns 3 :hint nil)
  "Jump To Line ->"
  ("u" avy-goto-line-above "Above")
  ("d" avy-goto-line-below "Below")
  ("s" avy-goto-line "Line Start")
  ("e" avy-goto-end-of-line "Line End")
  )

(defhydra gcl-jump-word (:color blue :columns 3 :hint nil)
  "Jump By Word ->"
  ("l" avy-jump-to-word-in-line "in line")
  ("w" avy-goto-word-1 "word1")
  ("0" avy-goto-word-0 "word0")
  ("a" avy-goto-word-0-above "above-0")
  ("A" avy-goto-word-1-above "above-1")
  ("b" avy-goto-word-0-below "below0")
  ("B" avy-goto-word-1-below "below1")
  ("o" avy-goto-word-or-subword-1 "word or subword")
  ("s" avy-subword-0 "subword-0")
  ("S" avy-subword-1 "subword-1")
  )

(defhydra gcl-dired (:color blue :hint nil)
  "
Mark              Operate         Misc              Navigate
----              -------         ----              --------
_fd_: flag del    _C_: copy       _+_: mkdir        _<up>_: up directory
_f#_: autosave    _R_: rename     _o_: open other
_f~_: backups     _D_: delete
_f&_: garbage     _F_: open marks
_fe_: extension
----
_m_: mark         _T_: touch
_/_: directories  _M_: chmod
_@_: symlinks     _G_: chgrp
_O_: omitted      _O_: chown
----
_U_: unmark all   _A_: find regx
_t_: toggle marks _Q_: find/rep
"
    ;; marking
  ("t" dired-toggle-marks)
  ("m" dired-mark :exit nil)
  ("u" dired-unmark :exit nil)
  ("fd" dired-flag-file-deletion)
  ("f#" dired-flag-auto-save-files)
  ("f~" dired-flag-backup-files)
  ("f&" dired-flag-garbage-files)
  ("fe" dired-flag-extension)
  ("/" dired-mark-directories)
  ("@" dired-mark-symlinks)
  ("." dired-mark-extension)
  ("O" dired-mark-omitted)
  ("U" dired-unmark-all-marks)

  ("C" dired-do-copy)
  ("R" dired-do-rename)
  ("D" dired-do-delete :exit nil)
  ("F" dired-do-find-marked-files)
  ("!" dired-do-shell-command)
  ("&" dired-do-async-shell-command)

  ("T" dired-do-touch)
  ("M" dired-do-chmod)
  ("G" dired-do-chgrp)
  ("O" dired-do-chown)

  ("A" dired-do-find-regexp)
  ("Q" dired-do-find-regexp-and-replace)

  ("+" dired-create-directory)
  ("o" dired-find-file-other-window)

  ("<up>" dired-up-directory)
  )

(defhydra hydra-movement ()
  ("j" next-line "down" :column "Vertical")
  ("k" previous-line "up")
  ("l" forward-char "forward" :column "Horizontal")
  ("h" backward-char "back"))

(defun org-agenda-cts ()
  (and (eq major-mode 'org-agenda-mode)
       (let ((args (get-text-property
                    (min (1- (point-max)) (point))
                    'org-last-args)))
         (nth 2 args))))

(defhydra gcl-agenda-view (:color blue :columns 3 :hint none)
    "
_d_: ?d? day        _g_: time grid=?g?  _a_: arch-trees
_w_: ?w? week       _[_: inactive       _A_: arch-files
_t_: ?t? fortnight  _f_: follow=?f?     _r_: clock report=?r?
_m_: ?m? month      _e_: entry text=?e? _D_: include diary=?D?
_y_: ?y? year       _q_: quit           _L__l__c_: log = ?l?"
    ("SPC" org-agenda-reset-view)
  ("d" org-agenda-day-view (if (eq 'day (org-agenda-cts)) "[x]" "[ ]"))
  ("w" org-agenda-week-view (if (eq 'week (org-agenda-cts)) "[x]" "[ ]"))
  ("t" org-agenda-fortnight-view (if (eq 'fortnight (org-agenda-cts)) "[x]" "[ ]"))
  ("m" org-agenda-month-view (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
  ("y" org-agenda-year-view (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
  ("l" org-agenda-log-mode (format "% -3S" org-agenda-show-log))
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("f" org-agenda-follow-mode (format "% -3S" org-agenda-follow-mode))
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode (format "% -3S" org-agenda-clockreport-mode))
  ("e" org-agenda-entry-text-mode (format "% -3S" org-agenda-entry-text-mode))
  ("g" org-agenda-toggle-time-grid (format "% -3S" org-agenda-use-time-grid))
  ("D" org-agenda-toggle-diary (format "% -3S" org-agenda-include-diary))
  ("!" org-agenda-toggle-deadlines)
  ("[" (let ((org-agenda-include-inactive-timestamps t))
         (org-agenda-check-type t 'timeline 'agenda)
         (org-agenda-redo)
         (message "Display now includes inactive timestamps as well")))
  ("q" (message "Abort") :exit t)
  ("v" nil)
  )

(defhydra gcl-verb-hydra (:colors yellow :columns 3 :hint nil)
  ("r" verb-send-request-on-point-other-window-stay "Send Focus")
  ("s" verb-send-request-on-point-other-window "Send Blur")
  ("f" verb-send-request-on-point "Fullscreen")
  ("m" verb-send-request-on-point-no-window "No Window")
  ("K" verb-kill-all-response-buffers "Kill All")

  ("vl" verb-show-vars "Show Vars")
  ("vl" verb-set-var "Set Var")
  ("vu" verb-unset-vars "Unset Vars")

  ("ec" verb-export-request-on-point-curl "Export Curl")
  ("ev" verb-export-request-on-point-verb "Export Verb")
  ("ew" verb-export-request-on-point-verb "Export EWW")
  )

(global-set-key (kbd "C-'") 'imenu-list-smart-toggle)

(use-package! js-doc
  :bind (:map js2-mode-map
         ("C-c i" . js-doc-insert-function-doc)
         ("@" . js-doc-insert-tag))
  :config
  (setq js-doc-mail-address user-mail-address
       js-doc-author (format "%s<%s>" user-full-name js-doc-mail-address)
       js-doc-url user-blog-url
       js-doc-license "MIT"))

(after! leetcode
  (setq leetcode-prefer-language "javascript"
        leetcode-prefer-sql "mysql"
        leetcode-save-solutions t
        leetcode-directory "~/github/make-leetcode"))

;; (use-package! link-hint
;;   :config
;;   (setq
;;    browse-url-browser-function 'browse-url
;;    ;; browse-url-generic-args '("--target" "tab")
;;    )
;;   )

(use-package! lsp-mode
  :hook ((web-mode . lsp)
         (rjsx-mode . lsp)
         (typescript-mode . lsp)
         ;; (vue-mode . lsp)
         (python-mode . lsp)
         (go-mode . lsp)
         (css-mode . lsp)
         (js2-mode . lsp)
         (bibtex-mode . lsp)
         (tex-mode . lsp)
         (latex-mode . lsp))
  :commands lsp
  :config
  (setq lsp-idle-delay 0.2
        lsp-enable-file-watchers nil))

(use-package! lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-headerline-breadcrumb-enable t ; 左上角显示文件路径
        lsp-lens-enable t                  ; 显示被引用次数
        )
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ([remap xref-pop-marker-stack] . lsp-ui-peek-jump-backward)
         ))

;; 关闭自动格式化，全局关闭
;; (setq +form-with-lsp nil)
;; 指定模式
;; (setq-hook! 'typescript-mode-hook +format-with-lsp nil)
;; (setq-hook! 'typescript-tsx-mode-hook +format-with-lsp nil)

(cl-defmacro lsp-org-babel-enable (lang)
"Support LANG in org source code block."
(setq centaur-lsp 'lsp-mode)
(cl-check-type lang stringp)
(let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
        (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
    (defun ,intern-pre (info)
        (let ((file-name (->> info caddr (alist-get :file))))
        (unless file-name
            (setq file-name (make-temp-file "babel-lsp-")))
        (setq buffer-file-name file-name)
        (lsp-deferred)))
    (put ',intern-pre 'function-documentation
            (format "Enable lsp-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
    (if (fboundp ',edit-pre)
        (advice-add ',edit-pre :after ',intern-pre)
        (progn
        (defun ,edit-pre (info)
            (,intern-pre info))
        (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))
(defvar org-babel-lang-list
'("go" "python" "ipython" "bash" "sh" "js" "typescript" "css"))
(dolist (lang org-babel-lang-list)
(eval `(lsp-org-babel-enable ,lang)))

;; (use-package! grip-mode
;;   :hook ((markdown-mode org-mode) . grip-mode)
;;   :config
;;   (setq grip-github-user "gcclll"
;;         grip-github-password "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"))

(use-package! pandoc-mode
  :after (markdown-mode org-mode)
  :hook
  (markdown-mode org-mode)
  (pandoc-mode . pandoc-load-default-settings))

(use-package! vmd-mode
  :after markdown-mode
  :bind
  (:map markdown-mode-map ("C-x p" . vmd-mode)))

(use-package! maple-iedit
   :commands (maple-iedit-match-all maple-iedit-match-next maple-iedit-match-previous)
   :config
   (delete-selection-mode t)
   (setq maple-iedit-ignore-case t)
   (defhydra maple/iedit ()
     ("n" maple-iedit-match-next "next")
     ("t" maple-iedit-skip-and-match-next "skip and next")
     ("T" maple-iedit-skip-and-match-previous "skip and previous")
     ("p" maple-iedit-match-previous "prev"))
   :bind (:map evil-visual-state-map
          ("n" . maple/iedit/body)
          ("C-n" . maple-iedit-match-next)
          ("C-p" . maple-iedit-match-previous)
          ("C-t" . map-iedit-skip-and-match-next)
          ("C-T" . map-iedit-skip-and-match-previous)))

(use-package! net-utils
  :bind
  (:map mode-specific-map
        :prefix-map net-utils-prefix-map ; C-c n x
        :prefix "n"
        ("p" . ping)
        ("i" . ifconfig)
        ("w" . iwconfig)
        ("n" . netstat)
        ("p" . ping)
        ("a" . arp)
        ("r" . route)
        ("h" . nslookup-host)
        ("d" . dig)
        ("s" . smbclient)
        ("t" . traceroute)))

(use-package! browse-at-remote
  :config
  (setq
   browse-at-remote-remote-type-domains '(("bitbucket.org" . "bitbucket")
                                          ("github.com" . "github")
                                          ("gitlab.com" . "gitlab")
                                          ("git.savannah.gnu.org" . "gnu")
                                          ("gist.github.com" . "gist")
                                          ("git.sr.ht" . "sourcehut")
                                          ("vs-ssh.visualstudio.com" . "ado")
                                          ("pagure.io" . "pagure")
                                          ("src.fedoraproject.org" . "pagure")
                                          ("code.aliyun.com" . "aliyun")
                                          )
))

;; (org-hide-emphasis-markers t)
(setq org-list-demote-modify-bullet
      '(("+" . "-")
        ("-" . "+")
        ("*" . "+")
        ("1." . "a.")))


;; (use-package! org
;;   :hook (
;;          ;; (verb-mode . org-mode)
;;          (+org-pretty-mode . org-mode)))

(after! org
  (add-hook 'org-mode-hook (lambda () (visual-line-mode -1)))

  (org-babel-do-load-languages 'org-babel-load-languages
                             (append org-babel-load-languages
                              '((restclient . t)
                                (verb . t))
                              ))

  (setq
   org-todo-keywords
   '((sequence "TODO(t)" "PROJECT(p)" "NEXT(n)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "SOMEDAY(s)" "MAYBE(m)" "|" "DONE(d)" "CANCELLED(c)")
     (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
     ;; (sequence "|" "OKAY(o)" "YES(y)" "NO(x)")
     )
   org-todo-keyword-faces `(("NEXT" . ,(doom-color 'green))
                            ("TODO" . ,(doom-color 'yellow))
                            ("PROJECT" . ,(doom-color 'tan))
                            ("WAIT" . ,(doom-color 'teal))
                            ("HOLD" . ,(doom-color 'red))
                            ("IDEA" . ,(doom-color 'tomato))
                            ;; ("OKAY" . ,(doom-color 'cyan))
                            ;; ,(if (eq doom-theme 'doom-vibrant)
                                 ;; (cons "OKAY" (doom-color 'base7))
                               ;; (cons "OKAY" (doom-color 'base5)))
                            ;; ("YES" . ,(doom-color 'blue))
                            ("SOMEDAY" . ,(doom-color 'base7))
                            ("MAYBE" . ,(doom-color 'base5))
                            ("[ ]" . ,(doom-color 'green))
                            ("[-]" . ,(doom-color 'yellow))
                            ("[?]" . ,(doom-color 'red))
                            )
   ;; org-enforce-todo-dependencies nil ;; if t, it hides todo entries with todo children from agenda
   ;; org-enforce-todo-checkbox-dependencies nil
   org-provide-todo-statistics t
   org-pretty-entities t
   org-hierarchical-todo-statistics t

   ;; org-startup-with-inline-images t
   org-hide-emphasis-markers t
   ;; org-fontify-whole-heading-line nil
   org-src-fontify-natively t
   org-imenu-depth 9

   org-use-property-inheritance t

   org-log-done 'time
   org-log-redeadline 'time
   org-log-reschedule 'time
   org-log-into-drawer "LOGBOOK"
   ;; org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"
   )
  )

(use-package! counsel-org-clock
  :commands (counsel-org-clock-context
             counsel-org-clock-history
             counsel-org-clock-goto)
  :config
  (setq counsel-org-clock-history-limit 20))

(use-package! engrave-faces-latex
  :after ox-latex)

(use-package! valign
  :custom
  (valign-fancy-bar t)
  :hook
  (org-mode . valign-mode))

;; (setq verb-base-headers '(("User-Agent" . "my-user-agent")))

;; unused: b,d,f,g,j,k,m,n,o,p,r,t,u,v,w,x,y,z
(use-package! verb
  :after org
  :config
  (setq
   tempo-template-org-verb '("#+begin_src verb :wrap src ob-verb-response"
                             nil '> n p n
                             "#+end_src" >)
   verb-auto-kill-response-buffers t)
  )

(after! org-clock
  (advice-add #'org-clock-in :after (lambda (&rest _)
                                      "Save all opened org-mode files."
                                      (org-save-all-org-buffers)))
  (advice-add #'org-clock-out :after (lambda (&rest _)
                                       "Save all opened org-mode files."
                                       (org-save-all-org-buffers)))
  (advice-add #'org-clock-load :around #'doom-shut-up-a)
  (advice-add #'org-clock-report :after (lambda (&rest _)
                                          "Save all opened org-mode files."
                                          (org-save-all-org-buffers)))
  (advice-add #'org-clock-goto :after (lambda (&rest _)
                                        "Narrow view after switching."
                                        (interactive)
                                        (widen)
                                        (+org-narrow-and-show)))

  (doom-store-persist "custom" '(org-clock-out-time))
  (setq
   org-clock-clocked-in-display nil
   org-clock-history-length 50
   org-clock-in-resume t
   org-clock-out-remove-zero-time-clocks t
   org-clock-persist t
   org-clock-persist-query-resume nil
   org-clock-report-include-clocking-task t
   )
  )

(use-package! org-chef
  :commands (org-chef-insert-recipe org-chef-get-recipe-from-url))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  )

(use-package! org-fancy-priorities
    :diminish
    :hook (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-fancy-priorities-list
          '("🅰" "🅱" "🅲" "🅳" "🅴")))

(add-hook 'org-mode-hook 'org-fragtog-mode)

(use-package! ox-gfm :after org)

(use-package! org-pandoc-import :after org)

(use-package! org-ol-tree
  :commands org-ol-tree)

(map! :map org-mode-map
    :after org
    :localleader
    :desc "Outline" "O" #'org-ol-tree)

(defun org-capture-select-template-prettier (&optional keys)
  "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
         (or (org-contextualize-keys
              (org-capture-upgrade-templates org-capture-templates)
              org-capture-templates-contexts)
             '(("t" "Task" entry (file+headline "" "Tasks")
                "* TODO %?\n  %u\n  %a")))))
    (if keys
        (or (assoc keys org-capture-templates)
            (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
               "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
               "Template key: "
               `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
(advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

(defun org-mks-pretty (table title &optional prompt specials)
  "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
  (save-window-excursion
    (let ((inhibit-quit t)
          (buffer (org-switch-to-buffer-other-window "*Org Select*"))
          (prompt (or prompt "Select: "))
          case-fold-search
          current)
      (unwind-protect
          (catch 'exit
            (while t
              (setq-local evil-normal-state-cursor (list nil))
              (erase-buffer)
              (insert title "\n\n")
              (let ((des-keys nil)
                    (allowed-keys '("\C-g"))
                    (tab-alternatives '("\s" "\t" "\r"))
                    (cursor-type nil))
                ;; Populate allowed keys and descriptions keys
                ;; available with CURRENT selector.
                (let ((re (format "\\`%s\\(.\\)\\'"
                                  (if current (regexp-quote current) "")))
                      (prefix (if current (concat current " ") "")))
                  (dolist (entry table)
                    (pcase entry
                      ;; Description.
                      (`(,(and key (pred (string-match re))) ,desc)
                       (let ((k (match-string 1 key)))
                         (push k des-keys)
                         ;; Keys ending in tab, space or RET are equivalent.
                         (if (member k tab-alternatives)
                             (push "\t" allowed-keys)
                           (push k allowed-keys))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                      ;; Usable entry.
                      (`(,(and key (pred (string-match re))) ,desc . ,_)
                       (let ((k (match-string 1 key)))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                         (push k allowed-keys)))
                      (_ nil))))
                ;; Insert special entries, if any.
                (when specials
                  (insert "─────────────────────────\n")
                  (pcase-dolist (`(,key ,description) specials)
                    (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                    (push key allowed-keys)))
                ;; Display UI and let user select an entry or
                ;; a sub-level prefix.
                (goto-char (point-min))
                (unless (pos-visible-in-window-p (point-max))
                  (org-fit-window-to-buffer))
                (let ((pressed (org--mks-read-key allowed-keys
                                                  prompt
                                                  (not (pos-visible-in-window-p (1- (point-max)))))))
                  (setq current (concat current pressed))
                  (cond
                   ((equal pressed "\C-g") (user-error "Abort"))
                   ;; Selection is a prefix: open a new menu.
                   ((member pressed des-keys))
                   ;; Selection matches an association: return it.
                   ((let ((entry (assoc current table)))
                      (and entry (throw 'exit entry))))
                   ;; Selection matches a special entry: return the
                   ;; selection prefix.
                   ((assoc current specials) (throw 'exit current))
                   (t (error "No entry available")))))))
        (when buffer (kill-buffer buffer))))))
(advice-add 'org-mks :override #'org-mks-pretty)

(use-package! doct
  :commands (doct))

(after! org-capture

  (defun +doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
    (let ((name (pop declaration))
          (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
          (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
          (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
      (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (defun +doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
      (setq doct-templates (mapcar (lambda (template)
                                     (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                 (spec (plist-get (plist-get props :doct) :icon)))
                                       (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                      "\t"
                                                                      (nth 1 template))))
                                     template)
                                   templates))))

  (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

  (defvar +org-capture-recipies  "~/.gclrc/org/recipies.org")

  (defun set-org-capture-templates ()
    (setq org-capture-templates
          (doct `(("Personal todo" :keys "t"
                   :icon ("checklist" :set "octicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a")
                   )
                  ("Personal note" :keys "n"
                   :icon ("sticky-note-o" :set "faicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Notes"
                   :type entry
                   :template ("* %?"
                              "%i %a"))
                  ("Emacs" :keys "e"
                   :icon ("emacs" :set "fileicon" :color "purple")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Emacs"
                   :type entry
                   :template ("* TODO %? :emacs:"
                              "%i %a"))
                  ("Email" :keys "E"
                   :icon ("envelope" :set "faicon" :color "blue")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                              "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                              "about %^{topic}"
                              "%U %i %a"))
                  ("Web" :keys "w"
                   :icon ("web" :set "material" :color "yellow")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Web"
                   :type entry
                   :template ("* TODO %{desc}%? :%{i-type}:"
                              "%i %a")
                   :children (("Vue" :keys "v"
                               :icon ("vue" :set "fileicon" :color "green")
                               :desc ""
                               :headline "Vue"
                               :i-type "web:vue")
                              ("React" :keys "r"
                               :icon ("react" :set "alltheicon" :color "blue")
                               :desc ""
                               :headline "React"
                               :i-type "web:react"
                               )
                              ("JavaScript" :keys "j"
                               :icon ("javascript-shield" :set "alltheicon" :color "yellow")
                               :desc ""
                               :i-type "web:javascript"
                               )
                              ("HTML" :keys "h"
                               :icon ("html5" :set "alltheicon" :color "orange")
                               :desc ""
                               :i-type "web:html"
                               )
                              ("CSS" :keys "c"
                               :icon ("css3" :set "alltheicon" :color "blue")
                               :desc ""
                               :i-type "web:css"
                               ))
                   )
                  ("Interesting" :keys "i"
                   :icon ("eye" :set "faicon" :color "lcyan")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Interesting"
                   :type entry
                   :template ("* [ ] %{desc}%? :%{i-type}:"
                              "%i %a")
                   :children (("Webpage" :keys "w"
                               :icon ("globe" :set "faicon" :color "green")
                               :desc "%(org-cliplink-capture) "
                               :i-type "read:web"
                               )
                              ("Links" :keys "l"
                               :icon ("link" :set "octicon" :color "blue")
                               :desc "%(org-cliplink-capture) "
                               :i-type "link:web"
                               )
                              ("Article" :keys "a"
                               :icon ("file-text" :set "octicon" :color "yellow")
                               :desc ""
                               :i-type "read:reaserch"
                               )
                              ("\tRecipie" :keys "r"
                               :icon ("spoon" :set "faicon" :color "dorange")
                               :file +org-capture-recipies
                               :headline "Unsorted"
                               :template "%(org-chef-get-recipe-from-url)"
                               )
                              ("Information" :keys "i"
                               :icon ("info-circle" :set "faicon" :color "blue")
                               :desc ""
                               :i-type "read:info"
                               )
                              ("Idea" :keys "I"
                               :icon ("bubble_chart" :set "material" :color "silver")
                               :desc ""
                               :i-type "idea"
                               )))
                  ("Tasks" :keys "k"
                   :icon ("inbox" :set "octicon" :color "yellow")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Tasks"
                   :type entry
                   :template ("* TODO %? %^G%{extra}"
                              "%i %a")
                   :children (("General Task" :keys "k"
                               :icon ("inbox" :set "octicon" :color "yellow")
                               :extra ""
                               )
                              ("Task with deadline" :keys "d"
                               :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                               :extra "\nDEADLINE: %^{Deadline:}t"
                               )
                              ("Scheduled Task" :keys "s"
                               :icon ("calendar" :set "octicon" :color "orange")
                               :extra "\nSCHEDULED: %^{Start time:}t"
                               )
                              ))
                  ("Project" :keys "p"
                   :icon ("repo" :set "octicon" :color "silver")
                   :prepend t
                   :type entry
                   :headline "Inbox"
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :file ""
                   :custom (:time-or-todo "")
                   :children (("Project-local todo" :keys "t"
                               :icon ("checklist" :set "octicon" :color "green")
                               :time-or-todo "TODO"
                               :file +org-capture-project-todo-file)
                              ("Project-local note" :keys "n"
                               :icon ("sticky-note" :set "faicon" :color "yellow")
                               :time-or-todo "%U"
                               :file +org-capture-project-notes-file)
                              ("Project-local changelog" :keys "c"
                               :icon ("list" :set "faicon" :color "blue")
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-project-changelog-file))
                   )
                  ("\tCentralised project templates"
                   :icon ("ionic-project" :set "fileicon" :color "cyan")
                   :keys "o"
                   :type entry
                   :prepend t
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :children (("Project todo"
                               :keys "t"
                               :prepend nil
                               :time-or-todo "TODO"
                               :heading "Tasks"
                               :file +org-capture-central-project-todo-file)
                              ("Project note"
                               :keys "n"
                               :time-or-todo "%U"
                               :heading "Notes"
                               :file +org-capture-central-project-notes-file)
                              ("Project changelog"
                               :keys "c"
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-central-project-changelog-file))
                   )))))

  (set-org-capture-templates)
  (unless (display-graphic-p)
    (add-hook 'server-after-make-frame-hook
              (defun org-capture-reinitialise-hook ()
                (when (display-graphic-p)
                  (set-org-capture-templates)
                  (remove-hook 'server-after-make-frame-hook
                               #'org-capture-reinitialise-hook))))))

(setf (alist-get 'height +org-capture-frame-parameters) 15)
;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
(setq +org-capture-fn
      (lambda ()
        (interactive)
        (set-window-parameter nil 'mode-line-format 'none)
        (org-capture)))

(after! org-agenda
  (advice-add #'org-agenda-archive :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-archive-default :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-refile :after (lambda (&rest _)
                                           "Refresh view."
                                           (if (string-match "Org QL" (buffer-name))
                                               (org-ql-view-refresh)
                                             (org-agenda-redo))))
  (advice-add #'org-agenda-redo :around #'doom-shut-up-a)
  (advice-add #'org-agenda-set-effort :after #'org-save-all-org-buffers)
  (advice-add #'org-schedule :after (lambda (&rest _)
                                      (org-save-all-org-buffers)))
  (advice-add #'org-deadline :after (lambda (&rest _)
                                      (org-save-all-org-buffers)))
  (advice-add #'+org-change-title :after (lambda (&rest _)
                                           (org-save-all-org-buffers)))
  (advice-add #'org-cut-special :after #'org-save-all-org-buffers)
  (advice-add #'counsel-org-tag :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-todo :after #'aj-org-agenda-save-and-refresh-a)
  (advice-add #'org-todo :after (lambda (&rest _)
                                  (org-save-all-org-buffers)))
  (advice-add #'org-agenda-kill :after #'aj-org-agenda-save-and-refresh-a)

  (setq
      org-agenda-prefix-format '((agenda    . "  %-6t %6e ")
                                 (timeline  . "  %-6t %6e ")
                                 (todo      . "  %-6t %6e ")
                                 (tags      . "  %-6t %6e ")
                                 (search    . "%l")
                                 )
      org-agenda-tags-column 80
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-timestamp-if-done t
      ;; org-agenda-todo-ignore-scheduled t
      ;; org-agenda-todo-ignore-deadlines t
      ;; org-agenda-todo-ignore-timestamp t
      ;; org-agenda-todo-ignore-with-date t
      org-agenda-start-on-weekday nil ; 从今天开始
      org-agenda-todo-list-sublevels t
      org-agenda-include-deadlines t
      org-agenda-log-mode-items '(closed clock state)
      org-agenda-block-separator nil
      org-agenda-compact-blocks t
      org-agenda-breadcrumbs-separator " ❱ "
      org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
      org-agenda-sorting-strategy
      '((agenda habit-down time-up effort-up priority-down category-keep)
        (todo   priority-up effort-up todo-state-up category-keep)
        (tags   priority-down category-keep)
        (search category-keep))
   )
  )

(use-package! org-super-agenda
  :commands (org-super-agenda-mode))

(after! org-agenda
  (org-super-agenda-mode))

(setq
 org-agenda-custom-commands
 '(("o" "Overview"
    ((agenda "" ((org-agenda-span 'day)
                 (org-super-agenda-groups
                  '((:name "Today"
                     :time-grid t
                     :date today
                     :todo "TODAY"
                     :scheduled today
                     :order 1)))))
     (alltodo
      ""
      ((org-agenda-overriding-header "")
       (org-super-agenda-groups
        '((:name "Next(接下来)"         :todo "NEXT"        :order 1)
          (:name "Important(重要)"     :tag "Important"    :order 2    :priority "A")
          (:name "Due Today(今天完成)" :deadline today     :order 3)
          (:name "Due Soon(很快过期)"  :deadline future    :order 8)
          (:name "Overdue(过期)"      :deadline past      :order 9    :face error)
          (:name "Emacs"             :tag "Emacs"        :order 10)
          (:name "Vue"               :tag "Vue"          :order 15)
          (:name "React"             :tag "React"        :order 18)
          (:name "Assignments(作业)"  :tag "Assignment"   :order 20)
          (:name "Waiting(等待)"      :todo "WAITING"     :order 21)
          (:name "To read(阅读)"      :tag "Read"         :order 25)
          (:name "Issues(问题)"       :tag "Issue"        :order 30)
          (:name "Projects(项目)"     :tag "Project"      :order 40)
          (:name "Research(研究)"     :tag "Research"     :order 50)
          (:name "University(综合)"   :tag "uni"          :order 60)
          (:name "Trivial(不重要)"
           :priority<= "E"
           :tag ("Trivial" "Unimportant")
           :todo ("SOMEDAY" )
           :order 90)
          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))

;; (use-package! org-roam-lib
;; :after org-roam)

(use-package! org-roam
  :config
  (setq
   org-roam-file-extensions '("txt" "org")
   org-roam-capture-templates
   (quote
    (("d" "default" plain
      (function org-roam-capture--get-point)
      "%?" :file-name "%<%Y_%m%d>_${slug}"
      :head "#+TITLE: ${title}\n\n" :unnarrowed t)))
   )
  (org-roam-setup)
  :bind (("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r g" . org-roam-graph)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ("C-c r j" . org-roam-dailies-capture-today)
         ))

(use-package! org-preview-html
  :after org)

(use-package! org-download
  :after org
  :bind
  (:map org-mode-map
   (("s-Y" . org-download-screenshot)
    ("s-y" . org-download-yank)))
  )

;; (use-package! mathpix.el
;;   :custom ((mathpix-app-id "app-id")
;;            (mathpix-app-key "app-key"))
;;   :bind
;;   ("C-x m" . mathpix-screenshot))

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

        ;; org
        (:rot ("DONE" "DOING" "WAITING" "PENDING"))
        (:rot ("increment", "decrement"))

        ))

(use-package! popper
  :bind
  ("C-`" . popper-toggle-latest)
  ("C-~" . popper-cycle)
  ("C-s-`" . popper-kill-latest-popup)
  :custom
  (popper-reference-buffers
   '("*eshell*"
     "*vterm*"
     "*color-rg*"
     "Output\\*$"
     "*Process List*"
     "COMMIT_EDITMSG"
     embark-collect-mode
     deadgrep-mode
     grep-mode
     rg-mode
     rspec-compilation-mode
     inf-ruby-mode
     nodejs-repl-mode
     ts-comint-mode
     compilation-mode))
  :config
  (defun zero-point-thirty-seven () 0.37)
  (advice-add 'popper-determine-window-height :override #'zero-point-thirty-seven)
  :init
  (popper-mode)
  )

(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

(use-package! projector
  :after
  (projectile vterm))

(map!
 (:prefix "s-p"
   "b"        #'projector-switch-to-shell-buffer
   "-"        #'projector-rerun-buffer-process

  (:prefix ("r" . "Run")
   "c"        #'projector-run-shell-command-current-directory-background
   "C"        #'projector-run-shell-command-current-directory
   "r"        #'projector-run-shell-command-project-root-background
   "R"        #'projector-run-shell-command-project-root
   )
  )
)

(after! ranger
  :config
  (setq ranger-show-literal nil))

(use-package! restclient
  :mode (("\\.rest\\'" . restclient-mode)
         ("\\.restclient\\'" . restclient-mode)))

(use-package! restclient-jq
  :after restclient)
(use-package! ob-restclient
  :after org restclient
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))

(use-package! smartparens
  :init
  (map! :map smartparens-mode-map
       "C-)" #'sp-forward-slurp-sexp
       "C-(" #'sp-forward-barf-sexp
       "C-{" #'sp-backward-slurp-sexp
       "C-}" #'sp-backward-barf-sexp
       ))

;; (use-package! server
;;  :unless (or noninteractive
;;              alternate-emacs)
;;  :no-require
;;  :config
;;  (unless (file-exists-p "/tmp/gcl-emacs")
;;    (make-directory "/tmp/gcl-emacs")
;;    (chmod "/tmp/gcl-emacs" 448))
;;  (setq server-socket-dir "/tmp/gcl-emacs")
;;  :hook (after-init . server-start))

(map!
 )

(after! treemacs
  (setq
   evil-treemacs-state-cursor 'box
   treemacs-project-follow-cleanup t
   treemacs-width 25
   )
  (treemacs-follow-mode +1)
  )

(use-package! visual-regexp
  :commands (vr/select-replace vr/select-query-replace))

(use-package! visual-regexp-steriods
  :commands (vr/select-replace vr/select-query-replace))

(after! which-key
  (setq! which-key-idle-delay 0.1
         which-key-idle-secondary-delay 0.2))

;; dont display evilem-...
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

(setq yas-triggers-in-field t)

(use-package! doom-snippets             ; hlissner
  :after yasnippet)

(use-package! yasnippet-snippets        ; AndreaCrotti
  :after yasnippet)

(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[a-z]+rc$" . conf-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("[._]bash.*" . shell-script-mode))

(defun maybe-use-prettier ()
  "Enable prettier-js-mode if an rc file is located."
  (if (locate-dominating-file default-directory ".prettierrc")
      (prettier-js-mode +1)))
(add-hook 'typescript-mode-hook 'maybe-use-prettier)
(add-hook 'js2-mode-hook 'maybe-use-prettier)
(add-hook 'web-mode-hook 'maybe-use-prettier)
(add-hook 'rjsx-mode-hook 'maybe-use-prettier)

;; set docsets
(after! (:any js-mode js2-mode rjsx-mode web-mode typescript-mode)
  (set-docsets! '(js-mode js2-mode rjsx-mode web-mode typescript-mode)
    "JavaScript" "AngularJS" "Bootstrap_4" "jQuery" "NodeJS" "React" "VueJS" "TypeScript"))

(use-package! jest
  :after js2-mode
  :config
  (advice-add #'jest--project-root :around (lambda (orig-fn &rest args)
                                             (if (string-match "exercism" (projectile-project-name))
                                                 (cl-letf (((symbol-function 'projectile-project-root)
                                                            (lambda (&rest _)
                                                              (file-name-directory buffer-file-name))))
                                                   (apply orig-fn args))
                                               (apply orig-fn args))))
  (setq jest-pdb-track nil)
  (add-hook 'jest-mode-hook (lambda ()
                              (evil-motion-state)
                              ))


  (set-popup-rule! "*jest\*"            :size 20            :side 'bottom :select t :quit t :modeline nil)
  )

(use-package! js-react-redux-yasnippets
  :after yasnippet)

(after! python
  (set-docsets! 'python-mode "Python_3")
  (set-popup-rule! "*Python*"     :size 16 :vslot -2 :side 'bottom :select t :quit t :ttl nil :modeline nil)
  )

(after! python-pytest
  (advice-add #'python-pytest--find-test-file
              :around
              (lambda (orig-fn &rest args)
                (if (string-match "exercism" (projectile-project-name))
                    (concat (file-name-sans-extension (buffer-file-name))
                            "_test.py")
                  (apply orig-fn args))))
  )
