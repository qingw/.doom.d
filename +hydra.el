;;; +hydra.el -*- lexical-binding: t; -*-


(defhydra hydra-main (:color blue :exit t :hint nil)
  "
all hydra apps:
------------------------------------------------------------------
 [_a_]   Tip          [_h_]   Launcher     [_m_]   Multiple Cursors
 [_w_]   Window       [_t_]   Text Zoom    [_o_]   Org Agenda
 [_j_]   Dump-Jump    [_e_]   Evil Mode
"
  ("a" hydra-tip/body)
  ("e" hydra-tip-evil/body)
  ("h" hydra-launcher/body)
  ("m" hydra-multiple-cursors/body)
  ("w" +hydra/window-nav/body)
  ("t" +hydra/text-zoom/body)
  ("o" hydra-org-agenda-view/body)
  ("j" hydra-dumb-jump/body)
  )

;; 提示面板
(defhydra hydra-tip (:color blue :hint nil)
  "
Tips for modes or kyes.
------------------------------------------------------------------
 [_m_]   M-Cursors   [_e_]   Evil    [_u_]   常用    [_q_] Quit
"
  ("m" hydra-tip-mcursors/body)
  ("u" hydra-tip-useful/body)
  ("e" hydra-tip-evil/body)
  ("w" hydra-web-mode/body)
  ("q" nil)
  )

;; 常用按键
(defhydra hydra-tip-useful (:color blue :hint nil)
  "
常用操作提示(C-Control, s-Command, M-option/alt)：
------------------------------------------------------------------
  括号操作          文本操作                    搜索/替换
------------------------------------------------------------------
 [C-(] 左括号左移   [s-<] move-text-up          [C-c r] 替换
 [C-)] 右括号右移   [s->] move-text-down        [C-c q] 搜索替换
 [s-)] 左括号右移   [C-+] 放大字体
 [s-(] 右括号左移   [C--] 缩小字体
 [z-]  取消括号     [M-u] 大写化
                    [M-l] 小写化
                    [M-c] 首字母大写
                    [M-i] 风格切换(java/ruby/py)
")

;; SPC 按键
(defhydra hydra-tip-spc (:hint nil)
  "
SPC 按键列表
------------------------------------------------------------------
        <a~l>                           <符号>
------------------------------------------------------------------
  [b O] kill-other-buffers          [RET]   加载书签
  [b m] 设置书签                    [']     恢复上一次搜索
  [i s] insert snippet
  [l m] lsp-ui-imenu
  [l t] treemacs
")

;; org-mode
(defhydra hydra-tip-org (:hint nil)
  "
Org-mode 按键提示
------------------------------------------------------------------
  Table 操作        跳转
------------------------------------------------------------------
  [M-l] 列右移      [gj] 上一个同级标题
  [M-h] 列左移      [gk] 下一个同级标题
  [M-j] 行下移      [gh] 父级标题
  [M-k] 行上移
")

;; evil 模式按键提示
(defhydra hydra-tip-evil (:hint nil)
  "
evil 模式下操作命令提示。
------------------------------------------------------------------
  符号/字母                     <z>
------------------------------------------------------------------
  [+]   数字+1                  [z-] 取消括号
  [-]   数字-1                  [z.] wrap 标签
  [K]   查文档                  [za] fold 所有
  [s/S] wrap 字符(选中)         [zo] open 当前
  [s/S] 文件内字符定位          [zj] fold 下一个
  [f/F] 行内字符定位            [zk] fold 上一个
  [t/T] 行内字符定位            [zr] open所有
  [;]   向后重复查找            [zm] close所有
  [,]   向前重复查找            [zt] 当前行定位到顶部
                                [zx] kill 当前buffer
------------------------------------------------------------------
                                <g>
------------------------------------------------------------------
 [_g[_] 函数开头                  [_gd_] 查找定义(definition)       [_g0_] 行首
 [_g]_] 函数结尾                  [_gD_] 查找引用(reference)
 [_gsj_] 按字符往后定位           [_gr_] 执行选中内容
 [_gss_] 按两个字符定位           [_gt_] 切换下一个workspace
 [_gs/_] 按单个字符定位           [_gx_] 交换两个选中区内容
 [_gsk_] 按字符往前定位           [_gf_] 查找光标处名称的文件
 [_gs[[_] 按段首向前定位
 [_gs[]_] 按断尾向前定位
 [_gs]]_] 按段首向后定位
 [_gs][_] 按断尾向后定位
"
  ("g[" beginning-of-defun)
  ("g]" end-of-defun)
  ("g0" evil-beginning-of-visual-line)
  ("gd" xref-find-definitions)
  ("gD" xref-find-references)
  ("gb" xref-pop-marker-stack)
  ("gr" +eval:region)
  ("gjj" dumb-jump-go)
  ("gjb" dumb-jump-back)
  ("gt" +workspace:switch-next)
  ("gx" evil-exchange)
  ("gf" +lookup/file)
  ("gss" evil-avy-goto-char-2)
  ("gs/" evil-avy-goto-char-timer)
  ("gsj" evilem-motion-next-line)
  ("gsk" evilem-motion-previous-line)
  ("gs[[" evilem-motion-backward-section-begin)
  ("gs[]" evilem-motion-backward-section-end)
  ("gs][" evilem-motion-forward-section-end)
  ("gs]]" evilem-motion-forward-section-begin)
  )

;; multiple cursors 按键提示
(defhydra hydra-tip-mcursors (:color blue :hint nil)
  "
Multiple Cursors Mode Tip(C-Control, S-Shift).
 [C-S-c 0] insert numbers   [C->] next
 [C-S-c 1] insert letters   [C->] previous
 [C-S-c s] region           [C-c C-<] all
 [C-S-c S] region regexp
 [C-S-c C-S-c] edit lines
")
(defhydra hydra-launcher (:color blue :hint nil :exit t :columns 3)
    "
快捷访问APP或网页:
------------------------------------------------------------------
"
  ("h" man "man")
  ("0" (browse-url "https://github.com/emacs-tw/awesome-emacs") "emacs awesome pkgs")
  ("1" (browse-url "https://github.com/caisah/emacs.dz" "emacs awesome config"))
  ("2" (browse-url "https://github.com/topics/emacs?l=emacs+lisp&o=desc&s=stars" "emacs github话题"))
  ("g" (browse-url "https://groups.google.com/my-groups") "google 论坛")
  ("l" (browse-url "https://langserver.org/") "Lsp server")
  ("L" (browse-url "https://emacs-lsp.github.io/lsp-mode/page/languages/") "lsp langs")
  ("m" (browse-url "https://exmail.qq.com/cgi-bin/frame_html?sid=diaeJi-xOyi0fVQL,7&sign_type=&r=906b84d4c87dfa71afab479b4a35e0ff") "腾讯企业邮")
  ("r" (browse-url "http://www.reddit.com/r/emacs") "reddit emacs")
  ("w" (browse-url "http://www.emacswiki.org/") "emacs wiki")
  ("z" (browse-url "https://www.zhihu.com/") "知乎")
  ("s" shell "shell")
  ("q" nil "quit"))

;; dumb-jump
(defhydra hydra-dumb-jump (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))

;; (defvar web-mode-title (with-octicon))
(defhydra hydra-web-mode (:color blue :quit-key "q" :hint nil)
  "
^Element^                       ^Element^                       ^Attribute^             ^Block
^^^^^^^^---------------------------------------------------------------------------------------------
_a_ : Select content            _r_ : Rename                    _0_ : Start             _<_ : Begin 
_b_ : Start                     _s_ : Select                    _9_ : End               _>_ : End
_c_ : Clone                     _t_ : Move Down                 _*_ : Insert            _-_ : Select
_e_ : End                       _u_ : Parent                    _N_ : Next                   
_f_ : Fold/unfold children      _v_ : Delete without content    _P_ : Previous                
_i_ : Insert                    _w_ : Wrap Element              _S_ : Select 
_I_ : Insert cursor             _t_ : Last(open/close)          _K_ : Delete
_k_ : Delete                    _T_ : Next(open/close)          _M_ : Match tag 
_n_ : Next                      _._ : Wrap Markup               _A_ : Sort
_p_ : Previous                  
"
 ("a" web-mode-element-content-select)
 ("b" web-mode-element-beginning)
 ("c" web-mode-element-clone)
 ("e" web-mode-element-end)
 ("f" web-mode-element-children-fold-or-unfold)
 ("F" web-mode-fold-unfold)
 ("i" web-mode-element-insert)
 ("I" web-mode-element-insert-at-point)
 ("k" web-mode-element-kill)
 ("m" web-mode-element-mute-blanks)
 ("n" web-mode-element-next :exit nil :color "pink")
 ("p" web-mode-element-previous :exit nil :color "pink")
 ("r" web-mode-element-rename)
 ("s" web-mode-element-select)
 ("t" web-mode-element-transpose)
 ("u" web-mode-element-parent :exit nil :color "pink")
 ("v" web-mode-element-vanish)
 ("w" web-mode-element-wrap)
 ("t" web-mode-tag-previous :exit nil :color "pink")
 ("T" web-mode-tag-next :exit nil :color "pink")
 ("." emmet-wrap-with-markup)
 ;; attribute
 ("0" web-mode-attribute-beginning)
 ("9" web-mode-attribute-end)
 ("*" web-mode-attribute-insert)
 ("A" web-mode-tag-attributes-sort)
 ("K" web-mode-attribute-kill)
 ("M" web-mode-tag-match :exit nil :color "pink")
 ("N" web-mode-attribute-next :exit nil :color "pink")
 ("P" web-mode-attribute-previous :exit nil :color "pink")
 ("S" web-mode-attribute-select)
 ;; block
 ("<" web-mode-block-next :exit nil :color "pink")
 (">" web-mode-block-previous :exit nil :color "pink")
 ("-" web-mode-block-select)
  )

(defhydra hydra-toggle (:color pink)
  "
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_f_ auto-fill-mode:    %`auto-fill-function
_t_ truncate-lines:    %`truncate-lines
_w_ whitespace-mode:   %`whitespace-mode
"
  ("a" abbrev-mode nil)
  ("d" toggle-debug-on-error nil)
  ("f" auto-fill-mode nil)
  ("t" toggle-truncate-lines nil)
  ("w" whitespace-mode nil)
  ("q" nil "quit"))

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                           :color pink
                           :post (deactivate-mark))
  "
  ^_k_^     _d_elete    _s_tring
_h_   _l_   _o_k        _y_ank
  ^_j_^     _n_ew-copy  _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _p_aste
"
  ("h" rectangle-backward-char nil)
  ("l" rectangle-forward-char nil)
  ("k" rectangle-previous-line nil)
  ("j" rectangle-next-line nil)
  ("e" hydra-ex-point-mark nil)
  ("n" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" kill-rectangle nil)
  ("o" nil nil))

(defhydra hydra-org-agenda-view (:hint none)
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
  ("v" nil))
