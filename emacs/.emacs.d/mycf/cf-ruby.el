(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-hook 'ruby-mode-hook #'aggressive-indent-mode)
(add-hook 'ruby-mode-hook (lambda() (local-set-key (kbd "C-c i r") 'inf-ruby)))
