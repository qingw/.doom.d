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
(defhydra hydra-launcher (:color blue :hint nil :exit t)
    "
all hydra apps or browse urls:
------------------------------------------------------------------
 [_g_] Google论坛      [_h_] Man                [_r_] Reddit
 [_w_] EmacsWiki        [_z_] Zhihu              [_m_] 腾讯企业邮箱
 [_s_] Shell            [_l_] LSP Server         [_L_] LSP Languages
 [_q_] Cancel
"
  ("h" man)
  ("g" (browse-url "https://groups.google.com/my-groups"))
  ("l" (browse-url "https://langserver.org/"))
  ("L" (browse-url "https://emacs-lsp.github.io/lsp-mode/page/languages/"))
  ("m" (browse-url "https://exmail.qq.com/cgi-bin/frame_html?sid=diaeJi-xOyi0fVQL,7&sign_type=&r=906b84d4c87dfa71afab479b4a35e0ff"))
  ("r" (browse-url "http://www.reddit.com/r/emacs"))
  ("w" (browse-url "http://www.emacswiki.org/"))
  ("z" (browse-url "https://www.zhihu.com/"))
  ("s" shell)
  ("q" nil))

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
^Element^                       ^Element^                       ^Attribute^             ^Block&Other
^^^^^^^^---------------------------------------------------------------------------------------------
_a_ : Select content            _r_ : Rename                    _0_ : Start             _<_ : Begin 
_b_ : Start                     _s_ : Select                    _9_ : End               _>_ : End
_e_ : End                       _t_ : Move Down                 _*_ : Insert            _-_ : Select
_f_ : Fold/unfold children      _u_ : Parent                    _K_ : Delete
_i_ : Insert                    _v_ : Delete without content    _N_ : Next
_I_ : Insert cursor             _w_ : Wrap Element              _P_ : Previous
_k_ : Delete                    _t_ : Last(open/close)          _S_ : Select
_n_ : Next                      _T_ : Next(open/close)
_p_ : Previous                  _._ : Wrap Markup
"
 ("a" web-mode-element-content-select)
 ("b" web-mode-element-beginning)
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
 ("K" web-mode-attribute-kill)
 ("N" web-mode-attribute-next :exit nil :color "pink")
 ("P" web-mode-attribute-previous :exit nil :color "pink")
 ("S" web-mode-attribute-select)
 ;; block
 ("<" web-mode-block-next :exit nil :color "pink")
 (">" web-mode-block-previous :exit nil :color "pink")
 ("-" web-mode-block-select)
  )
