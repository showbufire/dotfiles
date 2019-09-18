(require 'package)
;;; more: http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;; standard libraries
(require 'dired-x)
;; configure uniquify-buffer-name-style
;; http://www.emacswiki.org/emacs/uniquify
(require 'uniquify)

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
	(revert-buffer t t t) )))
  (message "Refreshed open files.") )

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
	    (kill-buffer))))))

(global-set-key (kbd "C-c d") 'delete-file-and-buffer)

(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))))

(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(global-set-key (kbd "C-c t") 'visit-term-buffer)

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(global-set-key (kbd "C-c ^") 'top-join-line)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-set-key (kbd "C-c b") 'my-insert-backtick)
(defun my-insert-backtick ()
  "Insert copyright header."
  (interactive)
  (insert "`"))

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; use sh-mode to load bashrc* files
(add-to-list 'auto-mode-alist '("\\.bash.*" . sh-mode))
;; use text-mode to open markdown files
(add-to-list 'auto-mode-alist '("\\.md\\'" . text-mode))
;; use js-mode to open .graphql files
(add-to-list 'auto-mode-alist '("\\.graphql\\'" . js-mode))

;; compile for prog-mode
(defun compile-kbd-hook ()
  (local-set-key (kbd "C-c C-c") 'compile))

(add-hook 'prog-mode-hook 'compile-kbd-hook)

(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages (quote (json-mode)))
 '(show-trailing-whitespace t)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify)))

;; turn off tabs
(setq-default indent-tabs-mode nil)

;; buck
(add-to-list 'auto-mode-alist '("BUCK\\'" . python-mode))

;; turn off vc backends because of mecurial is causing slowness
(setq vc-handled-backends nil)

;;; This file controls the list of packages installed.
;;; source: http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'json-mode)

;; activate installed packages
(package-initialize)

;;; For each package initialization

;; for markdown
(defun markdown-compile-hook ()
  (if  (string-match "md"
		     (file-name-extension buffer-file-name))
      (local-set-key (kbd "C-c C-c") 'gh-md-render-buffer)))

(add-hook 'text-mode-hook 'markdown-compile-hook)

;;; some variables

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
