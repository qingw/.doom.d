;;; zcheng/zhydra/config.el -*- lexical-binding: t; -*-

(defhydra hydra-main (:color blue :exit t :hint nil)
  "
all hydra apps:
------------------------------------------------------------------
 [_h_]   Launcher     [_m_]   Multiple Cursors    [_w_]   Window 
 [_t_]   Text Zoom    [_o_]   Org Agenda          
"
  ("h" hydra-launcher/body)
  ("m" hydra-multiple-cursors/body)
  ("w" +hydra/window-nav/body)
  ("t" +hydra/text-zoom/body)
  ("o" hydra-org-agenda-view/body)
  )

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

(defhydra hydra-multiple-cursors (:color blue :hint nil :exit nil)
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("s" mc/mark-all-in-region-regexp :exit t)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil))
