;; Disable package loading on startup
(setq package-enable-at-startup nil)

;; Defining function `termux-p'
(defun termux-p ()
  "Check whether Emacs running under Termux."
  (string-match-p
   (regexp-quote "/com.termux/")
   (expand-file-name "~")))
