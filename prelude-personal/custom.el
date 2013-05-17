(require 'package)
(add-to-list 'package-archives
                 '("marmalade" .
                         "http://marmalade-repo.org/packages/"))

(defun preview-in-marked-app ()
  "Open current file in Marked.app.  OS X only."
  (interactive)
  (cond ((eq system-type 'darwin)
         (call-process-shell-command
          (concat "open -a /Applications/Marked.app "
                  (shell-quote-argument buffer-file-name))))
        ('t (message "Marked.app is not available"))))

(add-hook 'markdown-mode-hook
          (lambda () (local-set-key (kbd "C-c C-e") #'preview-in-marked-app)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("60a2ebd7effefeb960f61bc4772afd8b1ae4ea48fae4d732864ab9647c92093a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
