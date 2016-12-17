(package-initialize nil)
(setq package-enable-at-startup nil)
(require 'org)
(org-babel-load-file "~/.emacs.d/Haoyang.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-curl-extra-arguments (quote ("-xhttp://alarmpi:8118")))
 '(package-selected-packages
   (quote
    (gruvbox-theme relative-line-numbers pyvenv company-jedi ox-gfm less-css-mode yasnippet js2-mode web-mode org-webpage haskell-mode intero org-bullets key-chord hydra f evil deft company-web company-ghci ace-window)))
 '(pyim-dicts nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
