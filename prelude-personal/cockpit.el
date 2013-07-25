;;;; Set up
(custom-set-variables
 '(custom-safe-themes (quote ("60a2ebd7effefeb960f61bc4772afd8b1ae4ea48fae4d732864ab9647c92093a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))

(defun imenu-elisp-sections ()
    (setq imenu-prev-index-position-function nil)
      (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

;;;; Custom functions
(defun init-imenu (p)
    (interactive "P")
      (find-file-existing "~/Codes/dotfiles/prelude-personal/custom.el")
        (widen)
          (helm-imenu)
            (if p (init-narrow-to-section)))

; Add space between linum and text
; from http://www.emacswiki.org/emacs/LineNumbers#toc7
(defvar my-linum-format-string "%4d")
(defun my-linum-get-format-string ()
  (let* ((width (length (number-to-string
                         (count-lines (point-min) (point-max)))))
         (format (concat "%" (number-to-string width) "d ")))
    (setq my-linum-format-string format)))

(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face 'linum))

; Use color to signify current evil-mode state
; Adapted from http://bit.ly/13ho93s (GitHub)
(defun my-propertized-evil-mode-tag ()
  (propertize evil-mode-line-tag 'font-lock-face
              ;; Don't propertize if we're not in the selected buffer
              (cond ((not (eq (current-buffer) (car (buffer-list)))) '())
                    ((eq evil-state 'normal) '(:foreground "yellow"))
                    ((eq evil-state 'replace)  '(:background "red" :foreground "white"))
                    ((eq evil-state 'emacs)  '(:background "red"))
                    ((eq evil-state 'motion) '(:background "orange"))
                    ((eq evil-state 'visual) '(:background "blue" :foreground "white"))
                    ((eq evil-state 'insert) '(:background "green"))
                    (t '()))))

(defun preview-in-marked-app ()
  "Open current file in Marked.app.  OS X only."
  (interactive)
  (cond ((eq system-type 'darwin)
         (call-process-shell-command
          (concat "open -a /Applications/Marked.app "
                  (shell-quote-argument buffer-file-name))))
        ('t (message "Marked.app is not available"))))

;;;; Look --- customize the interface

(load-theme 'zenburn)

(if (display-graphic-p nil)
    (progn ;; set English font
      (set-face-attribute 'default nil :font "Source Code Pro-12")
      ;; Chinese font
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset (font-spec :family "STHeiti"
                                             :size 12)))
      (setq face-font-rescale-alist '(("STHeiti" . 1.2)))))

(setq linum-format 'my-linum-format)
(setq-default mode-line-format (list "%e "
                             '(:eval (my-propertized-evil-mode-tag))
                             "%b "
                             mode-line-mule-info
                             mode-line-client
                             mode-line-modified
                             mode-line-remote
                             mode-line-frame-identification
                             " %P of %I "
                             " %l:%c "
                             '(vc-mode vc-mode)
                             " "
                             mode-line-modes
                             mode-line-misc-info
                             mode-line-end-spaces))

;;;; Environment
;; Use proxy if on a box in China
(if (string-equal system-type "darwin")
    (setq url-gateway-method 'socks))

(require 'package)
(add-to-list 'package-archives
                 '("marmalade" .
                         "http://marmalade-repo.org/packages/"))

;;;; Evil-mode
(require 'evil)
(evil-mode 1)
; disable key-chord mode
(key-chord-mode 0)
(global-linum-mode 1)

;;;; Org-mode
(require 'org)

(setq org-mobile-directory "/ssh:dev.idenizen.net#2121:/home/snakehsu/mobileorg")
(setq org-agenda-files (list
                        "~/org/office.org"
                        "~/org/personal.org"
                        "~/org/birthday.org"))
; capture settings
(setq org-default-notes-file (concat org-directory "/inbox.org"))
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

;;;; Hooks and mode-based keybindings
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)
(add-hook 'org-mode-hook (lambda () (linum-mode 0)))
(add-hook 'org-mode-hook (lambda () (whitespace-mode 0)))
(add-hook 'org-mode-hook (lambda () (define-key evil-normal-state-map
                                      (kbd "TAB") 'org-cycle)))
(add-hook 'markdown-mode-hook
          (lambda () (local-set-key (kbd "C-c C-e") #'preview-in-marked-app)))

;;;; Keybindings
(define-key global-map "\C-cc" 'org-capture)
(define-key evil-insert-state-map (kbd "RET") 'evil-ret-and-indent)

;;;; Customization through customize-*
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
