;; Disable package loading on startup
(setq package-enable-at-startup nil)

;; Load the core of Emacs
(load (expand-file-name
       (concat use-emacs-directory "core")))
