;; Enable Fira Code ligature support
(when (window-system)
  (set-frame-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               ;; (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; Disable ligatures in `helm-mode'
(add-hook 'helm-major-mode-hook
          (lambda ()
            (setq auto-composition-mode nil)))

;; Disable ligatures in `ediff-mode'
(add-hook 'ediff-mode-hook
          (lambda ()
            (setq auto-composition-mode nil)))

;; Disable Menu Bar.
(menu-bar-mode -1)

(setq-default

 ;; Don't use compiled code if it's older package.
 load-prefer-new t

 ;; Don't show startup message.
 inhibit-startup-screen t

 ;; Put 'customize' cofig in separate file.
 custom-file "~/.emacs.d/custom.el"

 ;; Don't create lockfiles.
 create-lockfiles nil

 ;; Don't use hard tabs.
 indent-tabs-mode nil

 ;; Create separate backup folder.
 backup-directory-alist '(("." . "~/.emacs.d/backups"))

 ;; Don't autosave.
 auto-save-default nil

 ;; Change intial scratch buffer messasge
 initial-scratch-message ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"

 ;; Allow commands to be run on minibuffers.
 enable-recursive-minibuffers t)

;; Load custom.el file.
(load-file custom-file)

;; Change all yes/no to y/n type.
(fset 'yes-or-no-p 'y-or-n-p)

;; Delete whitespace on saving file.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Display column number in modeline.
(column-number-mode t)

;; Auto revert buffer.
(global-auto-revert-mode t)

;; Setting for increasing LSP performance.
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

;; Change default server socket directory.
(require 'server)
(setq server-socket-dir "~/.emacs.d")

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

;; Enable transparency od current frame.
(when (display-graphic-p)
  (transparency 85))

;; Enable `display-line-numbers-mode' in `prog-mode'
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Enable `global-hl-line-mode'
(global-hl-line-mode t)
(set-face-foreground 'highlight nil)

;; Disable `hl-line' in `term-mode'
(add-hook 'term-mode-hook
          (lambda () (setq-local global-hl-line-mode nil)))

;; Enable `rainbow-delimiters'
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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

;; Setup `general' for leader key bindings
(use-package general
  :config
  (general-evil-setup)

  (general-create-definer leader-key-def
    :states 'normal
    :keymaps 'override
    :prefix "SPC"))

;; Setup `company' for code-completeion
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq
   company-idle-delay 0.500
   company-minimum-prefix-length 1))

;; Setup `smartparens' for auto pairing braces
(use-package smartparens
  :hook (prog-mode . smartparens-strict-mode))

;; Disable auto-pairing of single and double quotes
(require 'smartparens)
(sp-with-modes 'emacs-lisp-mode
               (sp-local-pair "'" nil :actions nil)
               (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p)))

;; Enable curly brace indentation
(defun create-nl-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(sp-local-pair 'prog-mode "{" nil :post-handlers '((create-nl-enter-sexp "RET")))

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

;; Setup `projectile'
(use-package projectile
  :init
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)))

;; Enable npm project detection
(use-package projectile
  :config
  (projectile-register-project-type
   'npm '("package.json")
   :project-file "package.json"
   :compile "npm install"
   :run "npm run"
   :test "npm test"
   :test-suffix ".spec"))

;; Enable `helm' with `projectile'
(use-package helm-projectile
  :requires (helm projectile)
  :init
  (helm-projectile-on))

;; Define custom keybindings
(leader-key-def
  "p" '(:ignore t :which-key "projectile")
  "pb" 'projectile-switch-to-buffer
  "pc" 'projectile-compile-project
  "pd" 'projectile-find-dir
  "pD" 'projectile-dired
  "pf" 'projectile-find-file
  "pk" 'projectile-kill-buffers
  "pL" 'projectile-install-project
  "pp" 'projectile-switch-project
  "pP" 'projectile-test-project
  "pS" 'projectile-save-project-buffers
  "pu" 'projectile-run-project
  "pT" 'projectile-find-test-file)

;; Setup `treemacs'
(use-package treemacs
  :bind
  (:map global-map
        ("<f9>" . treemacs)
        ("C-c <f9>" . treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window t))

;; Integrate `treemacs' with `evil'
(use-package treemacs-evil
  :after treemacs)

;; Integrate `treemacs' with `projectile'
(use-package treemacs-projectile
  :requires (treemacs projectile))

;; Integrate `treemacs' with `all-the-icons'
(use-package treemacs-all-the-icons)

;; Setup `magit'
(use-package magit
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Define custom keybindings
(leader-key-def
  "g" '(:ignore t :which-key "git")
  "gb" 'magit-branch
  "gc" 'magit-branch-or-checkout
  "gd" 'magit-diff-unstaged
  "gf" 'magit-fetch
  "gF" 'magit-fetch-all
  "gl" '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gp" 'magit-pull-branch
  "gP" 'magit-push-current
  "gr" 'magit-rebase
  "gs" 'magit-status)

;; Setup `forge'
(use-package forge
  :after magit
  :config
  (setq auth-sources '("~/.authinfo.gpg")))

;; Use system browser to browse url
(if (termux-p)
    (setq browse-url-browser-function 'browse-url-xdg-open)
  (setq browse-url-browser-function 'browse-url-chromium))

;; Enable `flycheck' for syntax checking
(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

;; Setup `lsp'
(use-package lsp-mode
  :commands lsp
  :hook (((js2-mode
           rjsx-mode
           html-mode
           css-mode
           json-mode) . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("TAB" . completion-at-point))
  :config
  (setq
   lsp-idle-delay 0.500
   lsp-headerline-arrow ""))

;; Integrate `helm' with `lsp'
(use-package helm-lsp
  :requires (lsp-mode helm)
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] 'helm-lsp-workspace-symbol))

;; Integrate `lsp' with `treemacs'
(use-package lsp-treemacs
  :requires (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

;; Define custom keybindigs for `lsp-mode'
(leader-key-def
  "l" '(:ignore t :which-key "lsp")
  "ld" 'lsp-find-definition
  "lr" 'lsp-find-references
  "ls" 'helm-imenu)

;; Setup `emmet-mode'
(use-package emmet-mode
  :straight (emmet-mode
             :fetcher github
             :repo "shaneikennedy/emmet-mode")
  :hook ((rjsx-mode
          mhtml-mode
          css-mode) . emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes 1))

;; Setup `restclient'
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

;; Use `company-restclient' as `company-backend' for `restclient-mode'
(use-package company-restclient
  :after company
  :config
  (add-to-list 'company-backends 'company-restclient))

;; Use `ob-restclient' for `org-babel' support
(use-package ob-restclient)

;; Setup `rainbow-mode'
(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         mhtml-mode
         css-mode
         js2-mode
         rjsx-mode))

;; Setup `js2-mode'
(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . js2-imenu-extras-mode))

;; Setup `rjsx-mode'
(use-package rjsx-mode
  :mode "\\.jsx\\'")

;; Expand `class' to `className' in `rjsx-mode'
(add-hook 'rjsx-mode-hook (lambda () (setq emmet-expand-jsx-className? t)))

;; Customize `org-ellipsis'
(use-package org
  :config
  (setq org-hide-emphasis-markers t))

;; Setup `org-bullets'
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

;; Add visual padding on both sides
(defun org-mode-visual-fill ()
  "Add padding on bith sides."
  (defvar visual-fill-column-width nil)
  (defvar visual-fill-column-center-text nil)
  (setq
   visual-fill-column-width (- (display-pixel-width) 4)
   visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

;; Indentation fix
(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0
      org-adapt-indentation t)

;; Add languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (restclient . t)))

;; Add templates for custom babel source block
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("rest" . "src restclient :pretty"))

;; Enable `evil-org'
(use-package evil-org
  :after org
  :hook (((org-mode org-agenda-mode) . evil-org-mode)
         (evil-org-mode . (lambda () (evil-org-set-key-theme
                                      '(navigation todo insert textobjects additional)))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

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
  "fed" '((lambda () (interactive) (find-file "~/.emacs.d/.emacs")) :which-key "emacs-dot-file")
  "feo" '((lambda () (interactive) (find-file "~/.emacs.d/emacs.org")) :which-key "emacs-org-file"))

;; Define buffer control bindings
(leader-key-def
  "b" '(:ignore t :which-key "buffers")
  "bb" 'helm-mini
  "bd" 'kill-current-buffer
  "bh" '((lambda () (interactive) (switch-to-buffer "*dashboard*")) :which-key "open-home-buffer")
  "bk" 'kill-buffer
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

;; Settings for `term-mode'
(use-package term
  :config
  (setq explicit-shell-file-name "zsh"))

;; Enable `eterm-256color-mode'
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))
