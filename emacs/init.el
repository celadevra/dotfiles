(package-initialize nil)
(setq package-enable-at-startup nil)
(require 'org)
(org-babel-load-file "~/.emacs.d/Haoyang.org")

(setq magit-last-seen-setup-instructions "1.4.0")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-tags-on-save t)
 '(pyim-dicts
   (quote
    ((:name "BigDict-01" :file "/Users/xhy/.emacs.d/pyim/dicts/pyim-bigdict.pyim.gz" :coding utf-8-unix :dict-type pinyin-dict)))))
