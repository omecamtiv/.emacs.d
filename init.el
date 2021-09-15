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

;; Load Emacs
(initialize-core)
(load-modules)
