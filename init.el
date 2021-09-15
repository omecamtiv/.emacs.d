;; System specific UI settings
(unless (termux-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10))

(menu-bar-mode -1)

(setq visible-bell t)

(if (display-graphic-p)
    (setq browse-url-browser-function 'browse-url-chromium))

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

;; Defining the function for auto tangle
(defun sn/org-babel-tangle-config ()
  "Tangle only `emacs.org' under ~/.emacs.d folder."
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/emacs.org"))
    ;; Dynamic scoping
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

;; Tangle when the file is saved
(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook
                               #'sn/org-babel-tangle-config)))

;; Load `config.el' file
(when (file-readable-p emacs-dot-file)
  (load-file emacs-dot-file))
