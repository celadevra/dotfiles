(package-initialize nil)
(setq package-enable-at-startup nil)
(require 'org)
(org-babel-load-file "~/.emacs.d/Haoyang.org")

(setq magit-last-seen-setup-instructions "1.4.0")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" default)))
 '(magit-diff-options nil)
 '(org-bullets-bullet-list (quote ("⦿" "❥" "▼" "❖" "✫" "✻")))
 '(send-mail-function (quote smtpmail-send-it))
 '(sml/battery-format " %b%p%%")
 '(sml/modified-char "x")
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
