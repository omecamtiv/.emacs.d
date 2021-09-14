;; Disable package loading on startup
(setq package-enable-at-startup nil)

;; Defining function `termux-p'
(defun termux-p ()
  "Check whether Emacs running under Termux."
  (string-match-p
   (regexp-quote "/com.termux/")
   (expand-file-name "~")))

;; Defining function `transparency'
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (set-frame-parameter (selected-frame) 'alpha value))
