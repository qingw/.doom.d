;;; +bindings.el -*- lexical-binding: t; -*-

;; unbind
(map! "C-s" nil
      "C-d" nil
      "M-," nil
      "M-." nil

      :leader
      "A" nil
      "X" nil)

;; global keys
(map! :ni       "C-="           #'er/expand-region
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

;; 11
;; 22
;; 33
