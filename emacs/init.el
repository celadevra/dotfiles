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
    ("74278d14b7d5cf691c4d846a4bbf6e62d32104986f104c1e61f718f9669ec04b" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "603a9c7f3ca3253cb68584cb26c408afcf4e674d7db86badcfe649dd3c538656" "40bc0ac47a9bd5b8db7304f8ef628d71e2798135935eb450483db0dbbfff8b11" "0820d191ae80dcadc1802b3499f84c07a09803f2cb90b343678bdb03d225b26b" "40c66989886b3f05b0c4f80952f128c6c4600f85b1f0996caa1fa1479e20c082" "959a77d21e6f15c5c63d360da73281fdc40db3e9f94e310fc1e8213f665d0278" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "790e74b900c074ac8f64fa0b610ad05bcfece9be44e8f5340d2d94c1e47538de" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" default)))
 '(erc-modules
   (quote
    (autojoin button completion dcc fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring stamp track)))
 '(erc-nick "snakehsu")
 '(erc-port 9999)
 '(erc-server "23.83.236.195")
 '(haskell-tags-on-save t)
 '(magit-diff-options nil)
 '(org-agenda-custom-commands
   (quote
    (("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)
     ("x" "All DONE/CANCELLED tasks" todo "DONE|CANCELLED" nil)
     ("u" "All unscheduled tasks" todo ""
      ((org-agenda-skip-function
	(lambda nil
	  (org-agenda-skip-entry-if
	   (quote scheduled)
	   (quote deadline)
	   (quote regexp)
	   "]+>"))))))))
 '(org-agenda-files (quote ("~/org/tasks.org")))
 '(org-babel-python-command "python3")
 '(org-bullets-bullet-list (quote ("⚀" "⚁" "⚂" "⚃" "⚄" "⚅")))
 '(org-medium-default-license "cc-40-by-sa")
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
