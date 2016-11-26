(setq user-full-name "Haoyang Xu")
(setq user-mail-address "github@expoundite.net")

(when (version<= emacs-version "24")
  (message "This config requires at least Emacs 24, please upgrade."))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(setq package-list '(ace-window
                     chinese-pyim
                     company
                     company-ghci
                     company-web
                     deft
                     elfeed
                     elfeed-org
                     ess
                     evil
                     evil-leader
                     evil-numbers
                     evil-paredit
                     evil-smartparens
                     exec-path-from-shell
                     flycheck-haskell
          ;           guide-key
                     haskell-mode
                     haskell-snippets
                     helm
                     helm-ag
                     helm-c-yasnippet
                     helm-dash
                     helm-git
                     helm-projectile
                     htmlize
                     jade-mode
                     js2-mode
                     lush-theme
                     magit
                     markdown-mode
                     nm
                     org-plus-contrib
                     org-bullets
                     org-ref
                     ox-pandoc
                     pandoc-mode
                     projectile
                     robe
                     smart-mode-line
                     vc-fossil
                     vc-darcs
                     yasnippet
                     yaml-mode))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq evil-want-C-i-jump nil)
(require 'evil-leader)
(global-evil-leader-mode) ; must be activated before evil-mode
(evil-leader/set-leader "<SPC>")

(require 'evil)
(evil-mode 1)

(evil-set-initial-state 'eww-mode 'emacs)
(evil-set-initial-state 'elfeed-search-mode 'emacs)
(evil-set-initial-state 'elfeed-show-mode 'emacs)
(evil-set-initial-state 'deft-mode 'emacs)

(global-visual-line-mode t)

(setq magit-auto-revert-mode nil)

(projectile-global-mode)

(setq backup-directory-alist '(("." . "~/backup")))
(setq version-control t)
(setq delete-old-versions t)

(call-process-shell-command "find ~/backup/* -mtime +5 -exec rm {} \\;" nil 0)

(exec-path-from-shell-initialize)

;; Setting rbenv path
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

(add-to-list 'vc-handled-backends 'Fossil)
(add-to-list 'vc-handled-backends 'darcs)

(setq browse-url-browser-function 'browse-url-default-browser)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode 0)
(setq inhibit-startup-screen 1)
(setq org-hide-emphasis-markers t)

(setq org-src-fontify-natively t)

(setq org-ellipsis " ⋯")

(add-hook 'after-init-hook
          (lambda ()
            (if (eq window-system nil)
              (load-theme 'lush t)
            (load-theme 'tango-dark t))))

(if (display-graphic-p nil)
    (progn ;; set English font
      (set-face-attribute 'default nil :font "Anonymous Pro-14")
      ;; Chinese font
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
        charset (font-spec :family "PingFang SC"
        :size 13)))
        (setq face-font-rescale-alist '(("PingFang SC" . 1.3)))))

(setq-default line-spacing 1)

; east asian ambiguous character table
(defun east-asian-ambiguous-characters ()
  '(
    (#x00A1 . #x00A1) (#x00A4 . #x00A4) (#x00A7 . #x00A8)
    (#x00AA . #x00AA) (#x00AD . #x00AE) (#x00B0 . #x00B4)
    (#x00B6 . #x00BA) (#x00BC . #x00BF) (#x00C6 . #x00C6)
    (#x00D0 . #x00D0) (#x00D7 . #x00D8) (#x00DE . #x00E1)
    (#x00E6 . #x00E6) (#x00E8 . #x00EA) (#x00EC . #x00ED)
    (#x00F0 . #x00F0) (#x00F2 . #x00F3) (#x00F7 . #x00FA)
    (#x00FC . #x00FC) (#x00FE . #x00FE) (#x0101 . #x0101)
    (#x0111 . #x0111) (#x0113 . #x0113) (#x011B . #x011B)
    (#x0126 . #x0127) (#x012B . #x012B) (#x0131 . #x0133)
    (#x0138 . #x0138) (#x013F . #x0142) (#x0144 . #x0144)
    (#x0148 . #x014B) (#x014D . #x014D) (#x0152 . #x0153)
    (#x0166 . #x0167) (#x016B . #x016B) (#x01CE . #x01CE)
    (#x01D0 . #x01D0) (#x01D2 . #x01D2) (#x01D4 . #x01D4)
    (#x01D6 . #x01D6) (#x01D8 . #x01D8) (#x01DA . #x01DA)
    (#x01DC . #x01DC) (#x0251 . #x0251) (#x0261 . #x0261)
    (#x02C4 . #x02C4) (#x02C7 . #x02C7) (#x02C9 . #x02CB)
    (#x02CD . #x02CD) (#x02D0 . #x02D0) (#x02D8 . #x02DB)
    (#x02DD . #x02DD) (#x02DF . #x02DF) (#x0300 . #x036F)
    (#x0391 . #x03A9) (#x03B1 . #x03C1) (#x03C3 . #x03C9)
    (#x0401 . #x0401) (#x0410 . #x044F) (#x0451 . #x0451)
    (#x2010 . #x2010) (#x2013 . #x2016) (#x2018 . #x2019)
    (#x201C . #x201D) (#x2020 . #x2022) (#x2024 . #x2027)
    (#x2030 . #x2030) (#x2032 . #x2033) (#x2035 . #x2035)
    (#x203B . #x203B) (#x203E . #x203E) (#x2074 . #x2074)
    (#x207F . #x207F) (#x2081 . #x2084) (#x20AC . #x20AC)
    (#x2103 . #x2103) (#x2105 . #x2105) (#x2109 . #x2109)
    (#x2113 . #x2113) (#x2116 . #x2116) (#x2121 . #x2122)
    (#x2126 . #x2126) (#x212B . #x212B) (#x2153 . #x2154)
    (#x215B . #x215E) (#x2160 . #x216B) (#x2170 . #x2179)
    (#x2190 . #x2199) (#x21B8 . #x21B9) (#x21D2 . #x21D2)
    (#x21D4 . #x21D4) (#x21E7 . #x21E7) (#x2200 . #x2200)
    (#x2202 . #x2203) (#x2207 . #x2208) (#x220B . #x220B)
    (#x220F . #x220F) (#x2211 . #x2211) (#x2215 . #x2215)
    (#x221A . #x221A) (#x221D . #x2220) (#x2223 . #x2223)
    (#x2225 . #x2225) (#x2227 . #x222C) (#x222E . #x222E)
    (#x2234 . #x2237) (#x223C . #x223D) (#x2248 . #x2248)
    (#x224C . #x224C) (#x2252 . #x2252) (#x2260 . #x2261)
    (#x2264 . #x2267) (#x226A . #x226B) (#x226E . #x226F)
    (#x2282 . #x2283) (#x2286 . #x2287) (#x2295 . #x2295)
    (#x2299 . #x2299) (#x22A5 . #x22A5) (#x22BF . #x22BF)
    (#x2312 . #x2312) (#x2460 . #x24E9) (#x24EB . #x254B)
    (#x2550 . #x2573) (#x2580 . #x258F) (#x2592 . #x2595)
    (#x25A0 . #x25A1) (#x25A3 . #x25A9) (#x25B2 . #x25B3)
    (#x25B6 . #x25B7) (#x25BC . #x25BD) (#x25C0 . #x25C1)
    (#x25C6 . #x25C8) (#x25CB . #x25CB) (#x25CE . #x25D1)
    (#x25E2 . #x25E5) (#x25EF . #x25EF) (#x2605 . #x2606)
    (#x2609 . #x2609) (#x260E . #x260F) (#x2614 . #x2615)
    (#x261C . #x261C) (#x261E . #x261E) (#x2640 . #x2640)
    (#x2642 . #x2642) (#x2660 . #x2661) (#x2663 . #x2665)
    (#x2667 . #x266A) (#x266C . #x266D) (#x266F . #x266F)
    (#x273D . #x273D) (#x2776 . #x277F) (#xE000 . #xF8FF)
    (#xFE00 . #xFE0F) (#xFFE0 . #xFFE6) (#xFFFD . #xFFFD)))

; setting function
(defun set-east-asian-ambiguous-width (width)
  (cond ((= emacs-major-version 22) (set-east-asian-ambiguous-width-22 width))
        ((> emacs-major-version 22) (set-east-asian-ambiguous-width-23 width))))

; for emacs 22
(defun set-east-asian-ambiguous-width-22 (width)
  (if (= width 2)
    (utf-translate-cjk-set-unicode-range (east-asian-ambiguous-characters))))

; for over 23 (checked work in emacs 24)
(defun set-east-asian-ambiguous-width-23 (width)
  (while (char-table-parent char-width-table)
         (setq char-width-table (char-table-parent char-width-table)))
  (let ((table (make-char-table nil)))
    (dolist (range (east-asian-ambiguous-characters))
      (set-char-table-range table range width))
    (optimize-char-table table)
    (set-char-table-parent table char-width-table)
    (setq char-width-table table)))

(set-east-asian-ambiguous-width 2)

(setq sml/no-confirm-load-theme t)
(setq sml/theme 'light)
(sml/setup)

(column-number-mode 1)
(display-battery-mode 1)

(smartparens-global-mode 1)
(show-smartparens-global-mode +1)

;; from http://stackoverflow.com/a/20788581
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(defun find-init-file () (interactive)
  "Find configuration files"
  (progn
    (delete-other-windows)
    (find-file "~/Codes/dotfiles/emacs/init.el")
    (find-file-other-window "~/Codes/dotfiles/emacs/Haoyang.org")))

(defun find-task-file () (interactive)
       "Find task file"
       (find-file "~/org/organizer.org"))

(defun find-notes-file () (interactive)
       "Find notes file"
       (find-file "~/org/organizer.org"))

(defun largest-issue-number ()
  "Find the largest number in issue tags"
  (let* ((issue-regexp ":issue[0-9]*:")
         (issues-list (re-seq issue-regexp 
                        (substring-no-properties (buffer-string)))))
    (if issues-list 
      (apply 'max (mapcar (lambda (str) (string-to-number str))
          (mapcar (lambda (str) (replace-regexp-in-string "[:isue]*" "" str)) issues-list)))
 0)))

; from http://emacs.stackexchange.com/questions/7148/get-all-regexp-matches-in-buffer-as-a-list
(defun re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun assign-issue-number ()
  "Assign issue number to heading."
  (interactive)
  (org-set-tags-to (cons (concat "issue" 
    (number-to-string (+ 1 (largest-issue-number)))) 
    (org-get-tags-at (point) t))))

(defun hy-org-tab ()
  "Part of the effort to make the <TAB> key behaviour
  context-dependent. In Org-mode
  and Evil Normal mode, fold/unfold the outline."
  (evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle))

(defun hy-enable-org-bullets ()
  "Only allow org-bullets in GUI environment, as many terms don't
know how to show UTF-8 chars correctly."
  (if (eq window-system nil)
      (progn
        (org-bullets-mode -1)
        (setq org-hide-leading-stars t))
    (org-bullets-mode 1)))

(defun hy-word-count ()
  "Calculate number of chars and words in the current buffer or active region."
  (interactive)
  (if (use-region-p)
      (message "%d chars, %d words" (abs (- (point) (mark)))
               (count-words-region (point) (mark)))
    (message "%d chars, %d words" (- (point-max) (point-min))
               (count-words-region (point-max) (point-min)))))

(evil-leader/set-key "x" 'helm-M-x)
(evil-leader/set-key "=" 'hy-word-count)
(evil-leader/set-key
  "gs" 'magit-status
  "gb" 'magit-checkout)
(evil-leader/set-key
  "dd" 'deft)
(evil-leader/set-key 
  "oc" 'org-capture
  "oa" 'org-agenda
  "ohh" 'helm-org-in-buffer-headings
  "ohc" 'helm-occur
  "ol" 'org-store-link
  "oL" 'org-insert-link
  "ob" 'ebib-handy
  "ot" 'org-todo-list
  "oi" 'assign-issue-number)
(evil-leader/set-key
  "ff" 'helm-find-files
  "fa" 'find-file-at-point
  "fi" 'find-init-file
  "fd" 'dired-at-point
  "fn" 'deft-find-file
  "fs" 'save-buffer
  "ft" 'find-task-file)
(evil-leader/set-key
  "h-" 'helm-dash-at-point
  "ha" 'helm-ag
  "hc" 'helm-occur
  "hd" 'helm-dash
  "hi" 'helm-imenu
  "hg" 'helm-projectile-ag
  "hp" 'helm-projectile)
(evil-leader/set-key
  "bb" 'helm-buffers-list
  "bd" 'kill-buffer)
(evil-leader/set-key
  "vv" 'vc-next-action)
(evil-leader/set-key
  "w0" 'delete-window
  "ww" 'ace-window
  "wv" 'split-window-horizontally
  "ws" 'split-window-vertically
  "wl" 'evil-window-right
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "w=" 'balance-windows)

(evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)

(async-shell-command "eval $(gpg-agent --daemon)" nil)

(setq notmuch-crypto-process-mime t)

(setq user-mail-address "haoyang@expoundite.net"
      user-full-name "Haoyang Xu")

(setq notmuch-fcc-dirs "INBOX.Sent")

(require 'smtpmail)
(require 'starttls)

(defun gnutls-available-p ()
  "Function redefined in order not to use built-in GnuTLS support"
  nil)
(setq starttls-gnutls-program "gnutls-cli")
(setq starttls-use-gnutls t)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-user "haoyang@fastmail.com"
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-service 587)

;; sign message by default
(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

(require 'elfeed)
(require 'elfeed-org)

(elfeed-org)

(setq rmh-elfeed-org-files (list "~/org/elfeed.org"))

(setq org-agenda-files (list 
                        "~/org/organizer.org"
                        "~/org/notes/"))

(setq org-todo-keywords
      '((sequence "TODO(t@/!)" "WAITING(w@/!)" "|" "DONE(d@/!)" "CANCELLED(c@/!)")))
(setq org-use-fast-todo-selection t)
(setq org-use-fast-tag-selection t)

(setq org-agenda-ndays 7)
(setq org-agenda-show-all-dates t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)

(setq org-reverse-note-order t)

(setq org-deadline-warning-days 14)

(setq org-default-notes-file (if (file-exists-p "~/org/") "~/org/organizer.org" "C:/Users/haoyang/Dropbox/org/tasks.org"))

(setq org-capture-templates
      '(("t" "Task" entry (file+headline org-default-notes-file "Inbox")
         "* TODO %^{Title}\n")
        ("b" "Bookmark" entry (file+headline org-default-notes-file "Bookmarks")
         "* %^{Title} %^g\n %^{URI} %?\n")
        ("n" "Work Notes" entry (clock)
         "* %^{Title}\n %U \n %^C \n\n %?")
        ("r" "Read Notes" entry (file+headline org-default-notes-file "Notes")
         "* %^{Title} %^g\n %^{URI|%x|%c} \n %?")
        ("q" "Quotes" entry (file+headline org-default-notes-file "Quotes")
         "* %^{Text|%x|%c} %^g\n --%^{Source}")
        ("s" "Snippet" entry (file+headline org-default-notes-file "Snippets")
         "* %^{Title} %^g\n %U \n #+BEGIN_SRC \n %^C \n #+END_SRC \n %?")))

(setq org-refile-targets '((nil . (:maxlevel . 6))))

(setq org-babel-load-languages
  '((sh . t)
    (emacs-lisp . t)
    (ruby . t)
    (R . t)
    (dot . t)
    (python . t)
    (haskell . t)))
(org-babel-do-load-languages 'l t)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-ref)

(setq reftex-default-bibliography '("~/org/bibliography/references.bib"))

(setq org-ref-bibliography-notes "~/org/bibliography/notes.org"
      org-ref-default-bibliography '("~/org/bibliography/references.bib")
      org-ref-pdf-directory "~/org/bibliography/bibtex-pdfs/")

(require 'ox-rss)
(setq org-publish-project-alist
      '(("expoundite.net" :components ("essays"
                                     "assets"
                                     "blog"
                                     "rss"))
      ("essays" :base-directory "~/org/publishing"
       :publishing-directory "~/org/published"
       :base-extension "org"
       :exclude "upload\.org\\|-draft-.*?\.org"
       :html-postamble t
       :recursive t
       :auto-sitemap t
       :html-doctype "html5"
       :html-mathjax-template "<script type=\"text/javascript\" src=\"%PATH\"></script>"
       :sitemap-sans-extension t
       :publishing-function org-html-publish-to-html)
      ("assets" :base-directory "~/org/publishing/assets"
       :base-extension any
       :publishing-directory "~/org/published"
       :publishing-function org-publish-attachment
       :recursive t)
      ("blog" :base-directory "~/org/publishing/blog"
       :publishing-directory "~/org/published/blog"
       :recursive t
       :with-toc nil
       :html-postamble t
       :html-doctype "html5"
       :html-head-extra "<link rel=\"alternate\" type=\"application/rss+xml\" href=\"https://expoundite.net/blog/rss.xml\" title=\"RSS Feed\"> 
                       <style type=\"text/css\"> 
                           h2 { font-size: 24px; } 
                           pre.example { background-color: rgba(255,255,255,255);
                                         border: none; }
                       </style>"
       :html-mathjax-template "<script type=\"text/javascript\" src=\"%PATH\"></script>"
       :publishing-function org-html-publish-to-html)
      ("rss" :base-directory "~/org/publishing/blog"
       :base-extension "org"
       :publishing-directory "~/org/published/blog"
       :publishing-function (org-rss-publish-to-rss)
       :exclude ".*"
       :include ("rss.org")
       :html-link-home "https://expoundite.net/blog"
       :html-link-use-abs-url t)))

(setq org-html-preamble-format
      '(("en" "<div class=\"leftside\" id=\"menu-closed\"><div>&#x2263;</div></div>
               <div class=\"middlesection\"></div>")))

(setq org-html-postamble-format
      '(("en" "<footer><p><a href=\"/\">Home</a> | <a href=\"/sitemap\">Site Map</a></p><hr>Created by <span class=\"author\"><a href=\"https://about.me/haoyangxu\">%a</a> (%e) on %d</span> <br>under <a href=\"https://creativecommons.org/licenses/by-sa/4.0/\">CC-BY-SA 4.0</a><p>Last Modified at %C</p></footer>")))

(setq org-html-head
      "<link rel=\"shortcut icon\" href=\"/favicon.ico\" type=\"image/x-icon\">
      <link rel=\"icon\" href=\"/favicon.ico\" type=\"image/x-icon\">
      <link href=\"https://fonts.googleapis.com/css?family=Sanchez|PT+Mono|Roboto:300\" rel=\"stylesheet\">
      <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/main.css\">
      <script src=\"/js/minified-web.js\" type=\"text/javascript\"></script>
      <script src=\"/js/main.js\" type=\"text/javascript\"></script>")

(setq org-html-mathjax-options
      '((path "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
        (scale 100)
        (align "center")
        (font "TeX")
        (linebreaks "false")
        (autonumber "AMS")
        (indent "0em")
        (multlinewidth "85%")
        (tagindent ".8em")
        (tagside "right")))

(add-to-list 'auto-mode-alist '("\\.page\\'" . org-mode))

(require 'chinese-pyim)

(setq default-input-method "chinese-pyim")
(global-set-key (kbd "C-\\") 'toggle-input-method)
;; use shuang pin
(setq pyim-default-pinyin-scheme 'pyim-shuangpin)

(require 'chinese-pyim-company)
(setq pyim-company-max-length 6)

(require 'deft)
(setq deft-directory "~/org/notes/")
(setq deft-recursive t)

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(require 'ess-site)

(add-hook 'after-init-hook 'global-company-mode)
(setq company-backend-list '(company-robe
                             company-web
                             company-capf))
(dolist (backend company-backend-list)
  (eval-after-load 'company
  '(push 'company-robe company-backends)))

(add-hook 'ruby-mode-hook 'robe-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(require 'haskell-mode)
(custom-set-variables
  '(haskell-tags-on-save t))
(define-key haskell-mode-map (kbd "C-t") 'haskell-mode-jump-to-def-or-tag)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq-default js2-basic-offset 2)

(add-to-list 'auto-mode-alist '("\\.mdwn\\'" . markdown-mode))

(add-hook 'markdown-mode-hook 'pandoc-mode)

(yas-global-mode 1)
