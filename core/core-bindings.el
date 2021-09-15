;; General keybindings.
(leader-key-def
  "SPC" 'helm-M-x)

;; Define keybindings for file handlings
(leader-key-def
  "f" '(:ignore t :which-key "files")
  "ff" 'helm-find-files
  "fF" 'find-file-at-point
  "fj" 'dired-jump
  "fl" 'find-file-literally
  "fr" 'helm-recentf
  "fs" '(save-buffer :which-key "save-current-file")
  "fS" '((lambda () (interactive) (save-some-buffers t nil)) :which-key "save-all-files")
  "fy" '((lambda () (interactive) (message buffer-file-name)) :which-key "current-file-path"))

;; Define some custom keybindings
(leader-key-def
  "fe" '(:ignore t :which-key "emacs-files")
  "fee" '((lambda () (interactive) (find-file early-init-file)) :which-key "early-init-file")
  "fei" '((lambda () (interactive) (find-file user-init-file)) :which-key "user-init-file")
  "feo" '((lambda () (interactive) (find-file emacs-org-file)) :which-key "emacs-org-file"))

;; Define buffer control bindings
(leader-key-def
  "b" '(:ignore t :which-key "buffers")
  "bb" 'helm-mini
  "bd" 'kill-current-buffer
  "bh" '((lambda () (interactive) (switch-to-buffer "*dashboard*")) :which-key "open-home-buffer")
  "bk" 'kill-buffer
  "br" 'revert-buffer
  "bs" '((lambda () (interactive) (switch-to-buffer "*scratch*")) :which-key "open-scratch-buffer"))

;; Define keybindings for killing emacs
(leader-key-def
  "q" '(:ignore t :which-key "quit")
  "qq" 'save-buffers-kill-emacs
  "qQ" 'kill-emacs
  "qs" '((lambda () (interactive) (save-buffers-kill-emacs t)) :which-key "auto-save-buffers-kill-emacs")
  "qz" '(delete-frame :which-key "kill-emacs-frame"))

;; Define keybindings for window control
(leader-key-def
  "w" '(:ignore t :which-key "window")
  "w=" 'balance-windows
  "w_" 'evil-window-set-height
  "wc" 'delete-other-windows
  "wC" 'evil-window-delete
  "wh" 'evil-window-left
  "wH" 'evil-window-move-far-left
  "wj" 'evil-window-down
  "wJ" 'evil-window-move-very-bottom
  "wk" 'evil-window-up
  "wK" 'evil-window-move-very-top
  "wl" 'evil-window-right
  "wL" 'evil-window-move-far-right
  "ws" 'evil-window-split
  "wv" 'evil-window-vsplit
  "ww" 'evil-window-next
  "wW" 'evil-window-prev)
