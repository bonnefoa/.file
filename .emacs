;; Afficher la 'parenthèse correspondante'
(require 'paren)
(show-paren-mode)

(global-font-lock-mode 1)
;; maximum colors
(setq font-lock-maximum-decoration t)
(setq-default indent-tabs-mode nil)

(setq default-frame-alist
      '((top . 0) (left . 0)
                  (width . 80) (height . 30)
                  ))

(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(add-to-list 'load-path "~/.emacs.d/")

(require 'init-auto-complete)

(require 'compile)
(setq compilation-finish-function
      (lambda (buf str)
        (unless (string-match "exited abnormally" str)
          ;;no errors, make the compilation window go away in a few seconds
          (run-at-time
            "2 sec" nil 'delete-windows-on
            (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!"))))

;; one-button testing, tada!
(global-set-key [f12] 'compile)


