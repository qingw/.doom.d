;;; zcheng/zhydra/config.el -*- lexical-binding: t; -*-

(defhydra hydra-main (:color blue :exit t :hint nil)
  "
all hydra apps:
------------------------------------------------------------------
 [_a_]   Tip          [_h_]   Launcher     [_m_]   Multiple Cursors
 [_w_]   Window       [_t_]   Text Zoom    [_o_]   Org Agenda          
"
  ("a" hydra-tip/body)
  ("h" hydra-launcher/body)
  ("m" hydra-multiple-cursors/body)
  ("w" +hydra/window-nav/body)
  ("t" +hydra/text-zoom/body)
  ("o" hydra-org-agenda-view/body)
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
  ("q" nil)
  )

;; 常用按键
(defhydra hydra-tip-useful (:color blue :hint nil)
  "
常用操作提示(C-Control, s-Command, M-option/alt)：
------------------------------------------------------------------
  括号操作          文本操作                    搜索/替换
------------------------------------------------------------------
 [C-(] 左括号左移   [s-<] move-text-up      [C-c r] 替换
 [C-)] 右括号右移   [s->] move-text-down    [C-c q] 搜索替换
 [s-)] 左括号右移   [C-+] 放大字体
 [s-(] 右括号左移   [C--] 缩小字体
 [z-]  取消括号     [M-u] 大写化
                    [M-l] 小写化
                    [M-c] 首字母大写
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
  符号/字母                 <g>                         <z>
------------------------------------------------------------------
  [+]   数字+1              [g[] 函数开头               [z-] 取消括号 
  [-]   数字-1              [g]] 函数结尾               [z.] wrap 标签
  [K]   查文档              [g~] 大小写切换             [za] fold 所有 
  [s/S] wrap 字符(选中)     [g0] 行首                   [zo] open 当前
  [s/S] 文件内字符定位      [gd] 查找定义(definition)   [zj] fold 下一个
  [f/F] 行内字符定位        [gD] 查找引用(reference)    [zk] fold 上一个
  [t/T] 行内字符定位        [gt] 切换下一个workspace   [zr] open所有
  [;]   向后重复查找        [gx] 交换两个选中区内容     [zm] close所有
  [,]   向前重复查找        [gf] 查找光标处名称的文件   [zt] 当前行定位到顶部
                            [gr] 执行选中内容           [zx] kill 当前buffer
                            [gss] 按两个字符定位
                            [gs/] 按单个字符定位
                            [gsj] 按字符往后定位
                            [gsk] 按字符往前定位
                            [gs[] 按section向前定位
                            [gs]] 按section向后定位
")

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
 [_h_]   Man     [_r_]   Reddit     [_w_]   EmacsWiki   [_z_]   Zhihu
 [_s_]   Shell   [_q_]   Cancel
"
  ("h" man)
  ("r" (browse-url "http://www.reddit.com/r/emacs"))
  ("w" (browse-url "http://www.emacswiki.org/"))
  ("z" (browse-url "https://www.zhihu.com/"))
  ("s" shell)
  ("q" nil))

