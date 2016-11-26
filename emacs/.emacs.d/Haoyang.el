(setq user-full-name "Haoyang Xu")
(setq user-mail-address "github@expoundite.net")

(when (version<= emacs-version "24")
(message "This config requires at least Emacs 24, please upgrade."))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package)

(setq delete-old-versions -1 ); delete excess backup versions silently
(setq version-control t ); use version control
(setq vc-make-backup-files t ); make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t ); inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore ); silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 ); use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil); sentence SHOULD end with only a point.
(setq default-fill-column 80); toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startup
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode 0)
(auto-fill-mode) ; toggle auto-fill-mode
(diminish auto-fill-function "F") ; diminish the hint to a simple "F"
(setq inhibit-startup-screen 1)
(setq org-hide-emphasis-markers t)
(setq package-check-singature nil)
(setf epa-pinentry-mode 'loopback)

(use-package evil :ensure t
             :config
             (evil-mode 1)
             (evil-set-initial-state 'eww-mode 'emacs)
             (evil-set-initial-state 'elfeed-search-mode 'emacs)
             (evil-set-initial-state 'elfeed-show-mode 'emacs)
             (evil-set-initial-state 'deft-mode 'emacs)
             :diminish undo-tree-mode)

(use-package swiper :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :diminish ivy-mode
)
(use-package counsel :ensure t)

(use-package magit :ensure t
  :diminish auto-revert-mode)

(use-package projectile :ensure t
  :config (projectile-global-mode)
  :diminish projectile-mode "P")

(use-package counsel-projectile :ensure t
  :config (counsel-projectile-on))

(use-package smartparens :ensure t
  :config
  (smartparens-global-mode t)
  (require 'smartparens-config)
  :diminish smartparens-mode)

(use-package rainbow-delimiters :ensure t
  :config
  (add-to-list 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package flycheck :ensure t
  ;; intero installs flycheck as a dep, but still
  :init (global-flycheck-mode))

(use-package general :ensure t
 :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "S-SPC"

   ;; simple commands
   "/" 'counsel-ag
   "x" 'counsel-M-x
   "U" 'counsel-unicode-char
   "#" 'ansi-term

   ;; applications
   "a" '(:ignore t :which-key "Applications")
   "ad" 'dired
   "ae" 'elfeed

   ;; buffer operations
   "b" '(:ignore t :which-key "Buffer commands")
   "bb" 'ivy-switch-buffer
   "bl" 'ibuffer
   "bd" 'evil-delete-buffer
   "bp" 'evil-prev-buffer
   "bn" 'evil-next-buffer

   ;; deft operations
   "d" '(:ignore t :which-key "Deft")
   "dd" 'deft
   "df" 'deft-find-file

   ;; file and dir commands
   "f"  '(:ignore t :which-key "File commands")
   "ff" 'counsel-find-file
   "fa" 'find-file-at-point
   "fp" 'counsel-projectile
   "fs" 'save-buffer
   "fr" 'counsel-recentf

   ;; git operations
   "g" '(:ignore t :which-key "Git commands")
   "gs" 'magit-status
   "gd" 'magit-diff
   "gl" 'magit-log

   ;; help and docs
   "h" '(:ignore t :which-key "Help and documentation")
   "hf" 'counsel-describe-function
   "hv" 'counsel-describe-variable
   "hw" 'woman
   "hi" 'counsel-info-lookup-symbol

   ;; org-mode
   "o" '(:ignore t :which-key "Org-mode")
   "oa" 'org-agenda
   "oc" 'org-capture
   "oe" 'org-edit-special
   "ol" 'org-store-link
   "oL" 'org-insert-link
   "oo" 'counsel-outline
   "op" 'owp/do-publication
   "ot" 'counsel-org-tag

   ;; window operations
   "w" '(:ignore t :which-key "Windows")
   "ww" 'ace-window
   "wo" 'delete-other-windows
   "wv" 'evil-window-vsplit
   "ws" 'evil-window-split
   "wj" 'evil-window-down
   "wk" 'evil-window-up
   "wl" 'evil-window-right
   "wh" 'evil-window-left)
  (general-define-key "C-s" 'swiper)
  (general-define-key :keymaps 'org-mode-map :states '(insert normal emacs) "TAB" 'org-cycle))
(use-package which-key :ensure t
  :config
  (which-key-mode 1)
  :diminish which-key-mode)

(use-package company :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-backend-list '(company-robe
                             company-web
                             company-jedi))
  :diminish company-mode)

(use-package yasnippet :ensure t
  :config
  (yas-global-mode t))

(if (eq window-system 'x)
  (progn ;; set English font
        (set-face-attribute 'default nil :font "Anonymous Pro-12")
        ;; Chinese font
        (dolist (charset '(kana han cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
          charset (font-spec :family "Source Han Sans CN"
          :size 18)))
          (setq face-font-rescale-alist '(("Source Han Sans CN" . 1.0)))))

(use-package darktooth-theme :ensure t
  :config (load-theme 'darktooth t nil))

(use-package telephone-line :ensure t
  :config
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))
  (telephone-line-mode 1))

(use-package org :ensure org-plus-contrib
  :config
  (progn
    (setq org-hide-emphasis-markers t) ; hide markers around bold/emphasis/delete etc, original value is nil.
    (setq org-ellipsis " ↲") ; more dense ellipsis, original value is '...'
    (setq org-agenda-files (list "~/org/organizer.org"))
    (setq org-todo-keywords
          '((sequence "TO-READ(r@)" "TO-WRITE(w@)" "TO-DO(t@)" "TO-LEARN(l@)" "WAITING(w@/!)" "|" "DONE(d@/!)" "CANCELLED(c@)")))
    (setq org-use-fast-todo-selection t)
    (setq org-use-fast-tag-selection t)
    (setq org-agenda-ndays 7)
    (setq org-agenda-show-all-dates t)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-start-on-weekday nil)
    (setq org-reverse-note-order t) ; notes attached to item sorted in date desc order
    (setq org-deadline-warning-days 14) ; depend on your pacing, default is 3
    (setq org-default-notes-file "~/org/organizer.org")
    (setq org-capture-templates
          '(("t" "Task" entry (file+headline org-default-notes-file "Inbox")
             "* TO-DO %^{Title}\n")
            ("b" "Bookmark" entry (file+headline org-default-notes-file "Bookmarks")
             "* TO-READ %^{Title} %^g\n %^{URI} %?\n")
            ("n" "Work Notes" entry (clock)
             "* %^{Title}\n %U \n %^C \n\n %?")
            ("r" "Read Notes" entry (file+headline org-default-notes-file "Notes")
             "* %^{Title} %^g\n %^{URI|%x|%c} \n %?")
            ("q" "Quotes" entry (file+headline "~/org/wiki/quotes.org" "Quotes")
             "* %^{Text|%x|%c} %^g\n --%^{Source}")
            ("s" "Snippet" entry (file+headline org-default-notes-file "Snippets")
             "* %^{Title} %^g\n %U \n #+BEGIN_SRC \n %^C \n #+END_SRC \n %?")))
    (setq org-refile-targets '((nil . (:maxlevel . 6)))) ; default is to maxlevel 2
    (setq org-babel-load-languages
          '((sh . t)
            (emacs-lisp . t)
            (ruby . t)
            (R . t)
            (dot . t)
            (python . t)
            (haskell . t))) ; these are the langs I work with
    (org-babel-do-load-languages 'l t) ; required for the above to work
      (defadvice org-html-paragraph (before fsh-org-html-paragraph-advice
                                          (paragraph contents info) activate)
      "Join consecutive Chinese lines into a single long line without 
unwanted space when exporting org-mode to html."
      (let ((fixed-contents)
            (orig-contents (ad-get-arg 1))
            (reg-han "[[:multibyte:]]"))
        (setq fixed-contents (replace-regexp-in-string
                              (concat "\$latex " reg-han
                                      "\$ *\n *\$latex " reg-han "\$")
                              "\\1\\2" orig-contents))
        (ad-set-arg 1 fixed-contents)))))

(use-package org-bullets :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

(use-package org-ref :ensure t
  :config
  (setq reftex-default-bibliography '("~/org/bibliography/references.bib"))
  (setq org-ref-bibliography-notes "~/org/bibliography/notes.org"
        org-ref-default-bibliography '("~/org/bibliography/references.bib")
        org-ref-pdf-directory "~/org/bibliography/bibtex-pdfs/")
  )

(require 'ox-rss)

(use-package org-webpage :ensure t
  :config
  (owp/add-project-config
   '("expoundite.net"
     :repository-directory "~/org/websrc"
     :remote (rclone "local" "/tmp/website")
     :site-domain "https://expoundite.net"
     :site-main-title "chmod +w Web"
     :site-sub-title "Essays"
     :ignore ("upload.org")
     :theme (worg)
     :default-category "essay"
     :source-browse-url ("GitHub" "https://github.com/celadevra/expoundite.net")
     :web-server-port 9999)))

(use-package markdown-mode :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.mdwn\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(use-package haskell-mode :ensure t)

(use-package intero :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package python :ensure t
  :config
  (setq-default python-shell-interpreter "ipython")
  (setq-default python-shell-buffer-name "IPython")
  (setq python-shell-interpreter-args "--simple-prompt --colors=linux")
  (setq python-shell-prompt-input-regexp "In \\[0-9]+\\]: ")
  (setq python-shell-prompt-output-regexp "Out\\[0-9]+\\]: ")
  (setq python-shell-completion-setup-code
     "from IPython.core.completerlib import module_completion"
     python-shell-completion-module-string-code
     "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code
     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

(use-package pyvenv :ensure t
  :config
  (add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python))

(use-package company-jedi :ensure t)

(use-package web-mode :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (setq web-mode-enable-current-element-highlight t)
  (defun my-web-mode-hook ()
    "hooks for webmode"
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook 'my-web-mode-hook))

(use-package js2-mode :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override t))

(use-package less-css-mode :ensure t)

(use-package deft :ensure t
  :config
  (setq deft-extensions '("md" "txt"))
  (setq deft-default-extension "md")
  (setq deft-directory "~/Notes")
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t))
