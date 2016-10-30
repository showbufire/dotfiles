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

(ensure-package-installed 'go-mode 'coffee-mode 'haskell-mode
			  'gh-md 'gradle-mode 'csharp-mode 'php-mode)

;; activate installed packages
(package-initialize)

;;; For each package initialization

;; for markdown
(defun markdown-compile-hook ()
  (if  (string-match "md"
		     (file-name-extension buffer-file-name))
      (local-set-key (kbd "C-c C-c") 'gh-md-render-buffer)))

(add-hook 'text-mode-hook 'markdown-compile-hook)

;; enable whitespace for js mode
(require 'whitespace)
(add-hook 'js-mode-hook
	  (lambda ()
	    (setq whitespace-line-column 80)
	    (setq whitespace-style '(face lines-tail))
	    (whitespace-mode t)))
