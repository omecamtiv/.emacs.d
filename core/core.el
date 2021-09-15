;; Define `emacs-org-file'
(defconst emacs-org-file
  (concat user-emacs-directory "emacs.org")
  "Path to `emacs.org' file.")

;; Define `emacs-core-directory'
(defconst emacs-core-directory
  (concat user-emacs-directory "core/"))

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

;; Define function `initialize-core'
(defun initialize-core ()
  "Load the all core files."
  (dolist (file '("core-package"
                  "core-lib"
                  "core-modules"
                  "core-bindings"))
    (load (expand-file-name
           (concat emacs-core-directory file)))
    (message "Loaded config file: %s" file)))

;; Define function `load-modules'
(defun load-modules ()
  "Load the modules for Emacs"
  (load (expand-file-name
         (concat user-emacs-directory "modules"))))
