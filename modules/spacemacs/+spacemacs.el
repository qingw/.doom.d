;;; +spacemacs.el -*- lexical-binding: t; -*-

;;; Setup & load spacmeacs core packages

(if (not (bound-and-true-p spacemacs-path))
    (setq spacemacs-path "~/.doom.d/modules/"))

(setq dotspacemacs-editing-style 'vim)
(setq dotspacemacs-emacs-command-key "SPC")
(setq dotspacemacs-emacs-leader-key "M-m")
(setq dotspacemacs-ex-command-key ":")
(setq dotspacemacs-leader-key "SPC")
(setq dotspacemacs-major-mode-leader-key ",")
(setq dotspacemacs-major-mode-emacs-leader-key "C-M-m")
(setq spacemacs-cache-directory doom-cache-dir)
(setq which-key-idle-delay 0.4)
(setq which-key-idle-secondary-delay 0.1)

(defun dotspacemacs/location ()
  "Dot file location - SPC-f-e-d"
  ;; (doom/find-file-in-private-config)
  (expand-file-name "config.el" doom-private-dir))

;; (setq dotspacemacs-persistent-server t)
(load! (concat spacemacs-path "spacemacs/core/core-funcs.el"))
; (load! (concat spacemacs-path "spacemacs/core/core-spacemacs-buffer.el")
;; (load! (concat spacemacs-path "spacemacs/core/core-fonts-support.el")
;; (load! (concat spacemacs-path "spacemacs/core/core-dumper.el")
(load! (concat spacemacs-path "spacemacs/core/core-keybindings.el"))
(load! (concat spacemacs-path "spacemacs/core/core-transient-state.el"))
(load! (concat spacemacs-path "spacemacs/core/core-toggle.el"))
;; (load! (concat spacemacs-path "spacemacs/core/core-hooks.el")
;; (load! (concat spacemacs-path "spacemacs/core/core-fonts-support.el")
;; TODO: evilified-state-evilify-map seems to have conflict with the doom
;; setting, like the leader key setting.
;; (use-package! evil-evilified-state
;;   :load-path
;;   (concat spacemacs-path "evil-evilified-state")

;; (require 'bind-map)
;; (require 'core-funcs)
;; (require 'core-keybindings)

(defvar dotspacemacs-distinguish-gui-tab nil
  "If non nil, distinguish C-i and tab in the GUI version of Emacs.")

(defvar spacemacs-post-user-config-hook nil
  "Hook run after dotspacemacs/user-config")
(defvar spacemacs-post-user-config-hook-run nil
  "Whether `spacemacs-post-user-config-hook' has been run")
(defvar dotspacemacs-show-transient-state-title t
  "If non nil show the titles of transient states.")
(defvar dotspacemacs-show-transient-state-color-guide t
  "If non nil show the color guide hint for transient state keys.")
(setq spacemacs-post-user-config-hook-run t)
(defun spacemacs/defer-until-after-user-config (func)
  "Call FUNC if dotspacemacs/user-config has been called. Otherwise,
defer call using `spacemacs-post-user-config-hook'."
  (if spacemacs-post-user-config-hook-run
      (funcall func)
    (add-hook 'spacemacs-post-user-config-hook func)))

(setq version-control-diff-tool 'git-gutter)


(defvar dotspacemacs-default-layout-name "Default"
  "Name of the default layout.")
(defvar dotspacemacs-auto-generate-layout-names nil
  "If non-nil, auto-generate layout name when creating new layouts.
Only has effect when using the \"jump to layout by number\" commands.")
(defun spacemacs/home-delete-other-windows ()
  "Open home Spacemacs buffer and delete other windows.
Useful for making the home buffer the only visible buffer in the frame."
  (interactive)
  (+doom-dashboard/open (selected-frame))
  (delete-other-windows))

;;; map

;; unmap the doom original key prefix
;; (map! :leader
;;       "gg" nil
;;       "bN" nil
;;       "bN" nil
;;       "fe" nil
;;       "fC" nil
;;       "fy" nil
;;       "hP" nil
;;       "hT" nil
;;       "tt" nil
;;       "wc" nil
;;       "x" nil)

(defmacro spacemacs/set-leader-keys (&rest rest)
  "redefine the spacemcas/set-leader-keys macro"
  ;; (setq doom--map-fn 'doom--define-leader-key)
  ;; (doom--map-keyword-to-states :n)
  ;; (setq doom--map-state '(:n t))
  ;; (message rest)
  (doom--map-process (cons :leader (cons :n rest) ))
  ;; (doom--map-process (cons '(:n) 'rest))
  ;; (doom--map-process (apply 'concat :n  rest))
  )

;;; Layers

;; load the modified spacemacs layers packages
;; initialise layers

;; vue layer
(load! (concat spacemacs-path "spacemacs/layer/vue/config.el"))
(load! (concat spacemacs-path "spacemacs/layer/vue/funcs.el"))
(load! (concat spacemacs-path "spacemacs/layer/vue/packages.el"))
