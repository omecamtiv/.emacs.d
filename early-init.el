;; Disable package loading on startup
(setq package-enable-at-startup nil)

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
