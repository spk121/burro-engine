;; Autocomplete
(setq load-path (cons "./auto-complete-1.3.1" (cons "." load-path)))
(require 'auto-complete-config)
(ac-config-default)
(require 'auto-complete-clang)
(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               "
 /home/mike/burro-engine2/src
 /usr/include/c++/4.8.3
 /usr/include/c++/4.8.3/i686-redhat-linux
 /usr/include/c++/4.8.3/backward
 /usr/include/4.8.3/include
 /usr/local/include
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
