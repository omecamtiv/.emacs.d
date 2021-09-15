;; Enable theme from `doom-themes'
(use-package doom-themes
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Enable `doom-modeline' and configure it
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq
   doom-modeline-buffer-file-name-style 'buffer-name
   doom-modeline-minor-modes nil
   doom-modeline-icon (display-graphic-p)
   doom-modeline-major-mode-icon t
   doom-modeline-major-mode-color-icon t
   doom-modeline-buffer-state-icon t
   doom-modeline-buffer-modification-icon t))

;; Enable `page-break-lines'
(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

;; Load and configure `dashboard'
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (if (display-graphic-p)
      (setq
       dashboard-startup-banner 'logo
       dashboard-set-heading-icons t)
    (setq dashboard-startup-banner 3))
  (setq
   dashboard-show-shortcuts nil
   dashboard-items '((recents . 5)
                     (bookmarks . 5)
                     (projects . 5))
   dashboard-page-separator "\n\f\n"
   dashboard-set-init-info t
   dashboard-set-footer t
   dashboard-set-navigator t
   dashboard-navigator-buttons
   '(((nil "GitHub" "GitHub Account"
           (lambda (&rest _) (browse-url "https://github.com/omecamtiv"))
           'dashboard-navigator "[" "]")
      (nil "Tutorial" "Emacs Tutorial"
           (lambda (&rest _) (help-with-tutorial))
           'dashboard-navigator "[" "]")
      (nil "About" "About Emacs"
           (lambda (&rest _) (about-emacs))
           'dashboard-navigator "[" "]")))))

;; Set `initial-buffer-choice' to load dashboard buffer
(setq initial-buffer-choice
      (lambda () (get-buffer "*dashboard*")))

;; Setup `ansi-color' in `compilation-mode'
(use-package ansi-color
  :config
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . colorize-compilation-buffer))

;; Disable `C-i' keybind in `evil-mode'
(defvar evil-want-C-i-jump nil)

;; Setup `evil'
(use-package evil
  :init (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; Enable `evil-collection'
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Escape from any state to `evil-normal-state'
(use-package evil-escape
  :config
  (evil-escape-mode)
  (setq-default evil-escape-delay 0.2))

;; Setup `which-key'
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-lighter nil))

;; Setup `helm'
(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-x r b" . helm-bookmarks)
   ("M-y" . helm-show-kill-ring))
  :config
  (helm-mode 1))

;; Setup `general' for leader key bindings
(use-package general
  :config
  (general-evil-setup)

  (general-create-definer leader-key-def
    :states 'normal
    :keymaps 'override
    :prefix "SPC"))

;; Use system browser to browse url
(if (termux-p)
    (setq browse-url-browser-function 'browse-url-xdg-open)
  (setq browse-url-browser-function 'browse-url-chromium))
