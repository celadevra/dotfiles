;;; behaviour.el --- customize the overall behaviour

;; Evil-mode
(require 'evil)
(evil-mode 1)
; disable key-chord mode
(key-chord-mode 0)
(global-linum-mode 1)

;; Customizing org-mode

(add-hook 'org-mode-hook (lambda () (linum-mode 0)))
(add-hook 'org-mode-hook (lambda () (whitespace-mode 0)))
(setq org-mobile-directory "/ssh:dev.idenizen.net#2121:/home/snakehsu/mobileorg")
(setq org-agenda-files '("~/org/office.org"))
