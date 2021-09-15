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
(setq server-socket-dir user-emacs-directory)
