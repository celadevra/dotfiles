;;; behaviour.el --- customize the overall behaviour

;; Evil-mode
(require 'evil)
(evil-mode 1)
; disable key-chord mode
(key-chord-mode 0)
(global-linum-mode 1)

;; Use proxy if on a box in China
(if (string-equal system-type "darwin")
    (setq url-gateway-method 'socks))
;; Customizing org-mode
(require 'org)

(add-hook 'org-mode-hook (lambda () (linum-mode 0)))
(add-hook 'org-mode-hook (lambda () (whitespace-mode 0)))
(add-hook 'org-mode-hook (lambda () (define-key evil-normal-state-map
                                      (kbd "TAB") 'org-cycle)))
(setq org-mobile-directory "/ssh:dev.idenizen.net#2121:/home/snakehsu/mobileorg")
(setq org-agenda-files (list
                        "~/org/office.org"
                        "~/org/personal.org"
                        "~/org/birthday.org"))
; capture settings
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(define-key global-map "\C-cc" 'org-capture)
; capture templates
(setq org-capture-templates
      '(("o" "Office Todo" entry (file+headline org-default-notes-file "Office INBOX")
         "* TODO %?\n %i\n %a")
        ("h" "Home Todo" entry (file+headline org-default-notes-file "Home INBOX")
         "* TODO %?\n %i\n %a")
        ("b" "Bugs" entry (file+headline org-default-notes-file "Bugs")
         "* TODO %?\n %i\n %F")
        ("j" "Journal" entry (file+headline (concat org-directory "/journal.org") "STREAM")
         "* %? %U %^g\n %i\n")))
; subscribe to RSS
(setq org-feed-alist
      '(("Aeon Magazine"
         "http://feeds.feedburner.com/AeonMagazineEssays"
         "~/org/feed.org" "Aeon Magazine")
        ("XKCD"
         "http://xkcd.com/rss.xml"
         "~/org/feed.org" "XKCD")))
; refile settings
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
(setq org-refile-use-outline-path 1)
