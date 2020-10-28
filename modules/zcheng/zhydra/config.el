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

(defhydra hydra-tip-useful (:color blue :hint nil)
  "
常用操作提示(C-Control, s-Command)：
------------------------------------------------------------------
  括号操作          文本操作                    搜索/替换
------------------------------------------------------------------
 [C-(] 左括号左移   [s-<] move-text-up      [C-c r] 替换
 [C-)] 右括号右移   [s->] move-text-down    [C-c q] 搜索替换
 [s-)] 左括号右移   [C-+] 放大字体
 [s-(] 右括号左移   [C--] 缩小字体
 [z-]  取消括号
")

(defhydra hydra-tip-evil (:hint nil)
  "
evil 模式下操作命令提示。
------------------------------------------------------------------
  符号              <g>             <z>
------------------------------------------------------------------
  [+] 数字+1    [g[] 函数开头      [z-] 取消括号 
  [-] 数字-1    [g]] 函数结尾      [z.] wrap 标签

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

