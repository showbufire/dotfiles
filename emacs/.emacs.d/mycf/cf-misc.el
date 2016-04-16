;; all those secondary major/minor mode related local configs are here

;; for markdown

(defun markdown-compile-hook ()
  (if  (string-match "md"
		     (file-name-extension buffer-file-name))
      (local-set-key (kbd "C-c C-c") 'gh-md-render-buffer)))

(add-hook 'text-mode-hook 'markdown-compile-hook)

;; compile for prog-mode
(defun compile-kbd-hook ()
  (local-set-key (kbd "C-c C-c") 'compile))

(add-hook 'prog-mode-hook 'compile-kbd-hook)
