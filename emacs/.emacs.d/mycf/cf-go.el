;;; source http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/

;;; PREREQUISITES:
;;; install go
;;; setup $GOPATH $GOROOT
;;; go get golang.org/x/tools/cmd/goimports
;;; go get golang.org/x/tools/cmd/oracle && sudo mv $GOPATH/bin/oracle $GOROOT/bin/


(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go generate && go build -v && go test -v && go vet"))
  (local-set-key (kbd "C-c f n") 'go-goto-function-name))
(add-hook 'go-mode-hook 'my-go-mode-hook)
;;; (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
