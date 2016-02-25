(require 'package)
;;; add marmalade
;;; more: http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;; standard libraries
(require 'dired-x)
;; configure uniquify-buffer-name-style
;; http://www.emacswiki.org/emacs/uniquify
(require 'uniquify)

;;; local config files
(add-to-list 'load-path "~/.emacs.d/mycf/")
(load "cf-packages")
(load "cf-global")
(load "cf-ruby")
(load "cf-go")

;;; some variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)
(setq tab-always-indent 'complete)

;; turn off annoying sound
(setq ring-bell-function 'ignore)
(setq visible-bell t)
