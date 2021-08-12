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
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

;; Change default server socket directory.
(require 'server)
(setq server-socket-dir "~/.emacs.d")

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight.el to install packages
(setq straight-use-package-by-default t)

;; Load helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Enable `doom-material' from `doom-themes'
(use-package doom-themes
  :config
  (load-theme 'doom-material t))

;; Enable `doom-modeline' and configure it
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq
   doom-modeline-buffer-file-name-style 'buffer-name
   doom-modeline-minor-modes nil))

;; Enable `display-line-numbers-mode' in `prog-mode'
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Enable `global-hl-line-mode' and change the color
(global-hl-line-mode t)
(set-face-background 'hl-line "#37474f")
(set-face-foreground 'highlight nil)
(set-face-attribute 'region nil :background "#1c262b")

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
  (setq
   dashboard-startup-banner 3
   dashboard-show-shortcuts nil
   dashboard-items '((recents . 5)
                     (bookmarks . 5)
                     (projects . 5))
   dashboard-page-separator "\n\f\n"
   dashboard-set-init-info t
   dashboard-set-footer t
   dashboard-set-navigator t
   dashboard-navigator-buttons
   '(((nil "Tutorial" "Emacs Tutorial"
           (lambda (&rest _) (help-with-tutorial))
           'dashboard-navigator "[" "]")
      (nil "About" "About Emacs"
           (lambda (&rest _) (about-emacs))
           'dashboard-navigator "[" "]")))))

;; Set `initial-buffer-choice' to load dashboard buffer
(setq initial-buffer-choice
      (lambda () (get-buffer "*dashboard*")))

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
   :run "npm start"
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
  :requires (treemacs evil))

;; Integrate `treemacs' with `projectile'
(use-package treemacs-projectile
  :requires (treemacs projectile))

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

;; Use `company-restclient' as `company-backed' for `restclient-mode'
(use-package company-restclient
  :requires (restclient company)
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
  (setq
   org-ellipsis " "
   org-hide-emphasis-markers t))

;; Setup `org-bullets'
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("")))

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

;; Defining the function for auto tangle
(defun sn/org-babel-tangle-config ()
  "Tangle only Emacs.org under ~/.emacs.d folder."
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/.emacs"))
    ;; Dynamic scoping
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

;; Tangle when the file is saved
(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook
                               #'sn/org-babel-tangle-config)))

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
  "fed" '((lambda () (interactive) (find-file "~/.emacs.d/.emacs")) :which-key "dotemacs-file")
  )

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
