;; haskell configs

;;; more about indentation
;;; https://github.com/haskell/haskell-mode/wiki/Indentation

;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(defun hsc-compile-hook ()
  (if (string-match "hsc"
		    (file-name-extension buffer-file-name))
      (let ((filename (file-name-nondirectory buffer-file-name)))
	(set (make-local-variable 'compile-command)
	     (format "hsc2hs %s" filename)))))

(add-hook 'haskell-mode-hook 'hsc-compile-hook)
