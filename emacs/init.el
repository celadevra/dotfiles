(package-initialize nil)
(setq package-enable-at-startup nil)
(require 'org)
(org-babel-load-file "~/.emacs.d/Haoyang.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-curl-extra-arguments (quote ("-xsocks5h://127.0.0.1:1080")))
 '(package-selected-packages
   (quote
    (darktooth-theme counsel chinese-pyim-greatdict org-bullets org key-chord ivy hydra f evil elfeed deft company-web company-ghci chinese-pyim ace-window)))
 '(pyim-dicts nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
