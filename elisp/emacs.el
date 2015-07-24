;;; Emacs is not a package manager, and here we load its package manager!
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ;; TODO: Maybe, use this after emacs24 is released
                  ;; (development versions of packages)
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))
(package-initialize)

;; Autocomplete
;; (setq load-path (cons "./auto-complete-1.3.1" (cons "." load-path)))
(require 'auto-complete-config)
(ac-config-default)
(require 'auto-complete-clang)
(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               "
 /home/mike/Projects/burro-engine/engine
 /usr/include
"
               )))
(define-key ac-mode-map  [(control tab)] 'ac-complete-clang)

;; C++ formatting
(require 'cc-mode)
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode nil)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; 
(require 'autopair)
(autopair-global-mode 1)
(set autopair-autowrap t)

;; Hide and show blocks
(add-hook 'c-mode-common-hook 'hs-minor-mode)

(require 'flymake)
;;(add-hook 'c-mode-common-hook 'flymake-mode)
(add-hook 'find-file-hook 'flymake-find-file-hook)
;; (require 'member-function)
;; (setq mf--source-file-extension "cpp")
