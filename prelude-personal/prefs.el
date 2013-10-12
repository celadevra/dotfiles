;;;; Set up
(defun imenu-elisp-sections ()
    (setq imenu-prev-index-position-function nil)
      (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

;;;; Custom functions
(defun init-imenu (p)
    (interactive "P")
      (find-file-existing "~/Codes/dotfiles/prelude-personal/prefs.el")
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
                  (shell-quote-argument (buffer-file-name)))))
        ('t (message "Marked.app is not available"))))

(defun org-install-series-time-stamp (a b)
  "For inserting a morning diary using org's template system.
At the function call, it calls org-insert-time-stamp to
insert a series of active time stamps, one line each. The
function is called with 2 parameters: a hour of the
work day and the b, in the form of int N and M.

i.e. (org-install-series-time-stamp 9 18)"
  ()
  (cond ((< b a) (org-install-series-time-stamp b a))
        ((< a (- b 1)) (concat (org-insert-time-stamp
                                 (current-time)
                                 nil nil nil nil
                                 (concat
                                  " " (number-to-string a) ":00-"
                                  (number-to-string (+ 1 a)) ":00"))
                         "\n" (make-string (org-outline-level) 42) " "
                         (org-install-series-time-stamp (+ 1 a) b)))
        ((= a (- b 1)) (concat (org-insert-time-stamp
                                 (current-time)
                                 nil nil nil nil
                                 (concat
                                  " " (number-to-string a) ":00-"
                                  (number-to-string (+ 1 a)) ":00"))
                                (org-install-series-time-stamp (+ 1 a) b)))
        ((= a b) ())))

; East Asian character width issue
(defun set-east-asian-ambiguous-width (width)
      (while (char-table-parent char-width-table)
              (setq char-width-table (char-table-parent char-width-table)))
          (let ((table (make-char-table nil)))
                  (dolist (range
                                  '(#x00A1 #x00A4 (#x00A7 . #x00A8) #x00AA (#x00AD . #x00AE)
                                           (#x00B0 . #x00B4) (#x00B6 . #x00BA) (#x00BC . #x00BF)
                                           #x00C6 #x00D0 (#x00D7 . #x00D8) (#x00DE . #x00E1) #x00E6
                                           (#x00E8 . #x00EA) (#x00EC . #x00ED) #x00F0
                                           (#x00F2 . #x00F3) (#x00F7 . #x00FA) #x00FC #x00FE
                                           #x0101 #x0111 #x0113 #x011B (#x0126 . #x0127) #x012B
                                           (#x0131 . #x0133) #x0138 (#x013F . #x0142) #x0144
                                           (#x0148 . #x014B) #x014D (#x0152 . #x0153)
                                           (#x0166 . #x0167) #x016B #x01CE #x01D0 #x01D2 #x01D4
                                           #x01D6 #x01D8 #x01DA #x01DC #x0251 #x0261 #x02C4 #x02C7
                                           (#x02C9 . #x02CB) #x02CD #x02D0 (#x02D8 . #x02DB) #x02DD
                                           #x02DF (#x0300 . #x036F) (#x0391 . #x03A9)
                                           (#x03B1 . #x03C1) (#x03C3 . #x03C9) #x0401
                                           (#x0410 . #x044F) #x0451 #x2010 (#x2013 . #x2016)
                                           (#x2018 . #x2019) (#x201C . #x201D) (#x2020 . #x2022)
                                           (#x2024 . #x2027) #x2030 (#x2032 . #x2033) #x2035 #x203B
                                           #x203E #x2074 #x207F (#x2081 . #x2084) #x20AC #x2103
                                           #x2105 #x2109 #x2113 #x2116 (#x2121 . #x2122) #x2126
                                           #x212B (#x2153 . #x2154) (#x215B . #x215E)
                                           (#x2160 . #x216B) (#x2170 . #x2179) (#x2190 . #x2199)
                                           (#x21B8 . #x21B9) #x21D2 #x21D4 #x21E7 #x2200
                                           (#x2202 . #x2203) (#x2207 . #x2208) #x220B #x220F #x2211
                                           #x2215 #x221A (#x221D . #x2220) #x2223 #x2225
                                           (#x2227 . #x222C) #x222E (#x2234 . #x2237)
                                           (#x223C . #x223D) #x2248 #x224C #x2252 (#x2260 . #x2261)
                                           (#x2264 . #x2267) (#x226A . #x226B) (#x226E . #x226F)
                                           (#x2282 . #x2283) (#x2286 . #x2287) #x2295 #x2299 #x22A5
                                           #x22BF #x2312 (#x2460 . #x24E9) (#x24EB . #x254B)
                                           (#x2550 . #x2573) (#x2580 . #x258F) (#x2592 . #x2595)
                                           (#x25A0 . #x25A1) (#x25A3 . #x25A9) (#x25B2 . #x25B3)
                                           (#x25B6 . #x25B7) (#x25BC . #x25BD) (#x25C0 . #x25C1)
                                           (#x25C6 . #x25C8) #x25CB (#x25CE . #x25D1)
                                           (#x25E2 . #x25E5) #x25EF (#x2605 . #x2606) #x2609
                                           (#x260E . #x260F) (#x2614 . #x2615) #x261C #x261E #x2640
                                           #x2642 (#x2660 . #x2661) (#x2663 . #x2665)
                                           (#x2667 . #x266A) (#x266C . #x266D) #x266F #x273D
                                           (#x2776 . #x277F) (#xE000 . #xF8FF) (#xFE00 . #xFE0F)
                                           #xFFFD
                                           ))
                    (set-char-table-range table range width))
                        (optimize-char-table table)
                              (set-char-table-parent table char-width-table)
                                    (setq char-width-table table)))
(defvar powerline-separator 'arrow
  "Shape of separator used by powerline-my-evil-theme")
(defface my-powerline-active1 '((t (:background "DeepSkyBlue4" :inherit mode-line)))
  "custom Powerline face 1"
  :group 'powerline)
(defface my-powerline-active2 '((t (:background "SeaGreen" :inherit mode-line)))
  "custom Powerline face 2"
  :group 'powerline)
(defface my-powerline-inactive2 '((t (:background "LightSlateGray" :inherit mode-line)))
  "custom Powerline face 2"
  :group 'powerline)
(defface my-powerline-inactive1 '((t (:background "gray36" :inherit mode-line)))
  "custom Powerline face 1"
  :group 'powerline)

(defun powerline-my-evil-theme ()
  "Set up the powerline for evil-mode"
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'my-powerline-active1 'my-powerline-inactive1))
                          (face2 (if active 'my-powerline-active2 'my-powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw (my-propertized-evil-mode-tag) nil 'l)
                                     (funcall separator-left mode-line face1)
                                     (powerline-raw "%b " face1 'l)
                                     (powerline-raw mode-line-modified face1 'l)
                                     (funcall separator-left face1 face2)
                                     (powerline-buffer-size face2 'l)
                                     (powerline-narrow face2 'l)
                                     (powerline-hud face1 face2)
                                     (powerline-narrow face2 'l)
                                     (powerline-vc face2)))
                          (rhs (list (powerline-major-mode face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-minor-modes nil 'r))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(defvar default-im "com.apple.keylayout.Dvorak" "Default ascii-only input method")
(defvar prev-im (substring (shell-command-to-string "/Users/snakehsu/bin/im-select") 0 -1) "IM that I use when starting Emacs and exiting insert mode")
(defun im-use-dvorak ()
  "Switch to Dvorak input method on a Mac. im-select is a tool provided at http://git.io/ndA8Mw"
  (interactive)
  (cond ((eq system-type 'darwin)
         (call-process-shell-command (concat "/Users/snakehsu/bin/im-select " default-im)))))

(defun im-remember ()
  "Remember the input method being used in insert mode, so we can switch to it in other modes."
  (interactive)
  (cond ((eq system-type 'darwin)
         (setq prev-im (substring (shell-command-to-string "/Users/snakehsu/bin/im-select") 0 -1)))))

(defun im-use-prev ()
  "Use previous input method. If previous input method is not defined, use default method"
  (interactive)
  (cond ((eq system-type 'darwin)
         (if prev-im
             (call-process-shell-command (concat "/Users/snakehsu/bin/im-select " prev-im))
           (call-process-shell-command (concat "/Users/snakehsu/bin/im-select " default-im))))))

;;;; Look --- customize the interface

(load-theme 'zen-and-art)

(if (display-graphic-p nil)
    (progn ;; set English font
      (set-face-attribute 'default nil :font "Anonymous Pro-12")
      ;; Chinese font
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset (font-spec :family "STFangsong"
                                             :size 12)))
      (setq face-font-rescale-alist '(("STFangsong" . 1.2)))))

(setq linum-format 'my-linum-format)
;(require 'powerline)
(setq powerline-separator 'contour)
(setq powerline-height 20)
(powerline-my-evil-theme)
;; (setq-default mode-line-format (list "%e "
;;                              '(:eval (my-propertized-evil-mode-tag))
;;                              "%b "
;;                              mode-line-mule-info
;;                              mode-line-client
;;                              mode-line-modified
;;                              mode-line-remote
;;                              mode-line-frame-identification
;;                              " %P of %I "
;;                              " %l:%c "
;;                              '(vc-mode vc-mode)
;;                              " "
;;                              mode-line-modes
;;                              mode-line-misc-info
;;                              mode-line-end-spaces))
(set-east-asian-ambiguous-width 2)

;;;; Environment
;; Use proxy if on a box in China
(set-language-environment "UTF-8")
(if (string-equal system-type "darwin")
    (setq url-gateway-method 'socks))

(require 'package)
(add-to-list 'package-archives
                 '("marmalade" .
                         "http://marmalade-repo.org/packages/"))
;; Don't let whitespace-mode whine about longer lines
(setq whitespace-style '(face tabs empty trailing))

;;;; Auto-Complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-quick-help-delay 1)

;;;; Evil-mode
(require 'evil)
(evil-mode 1)
; disable key-chord mode
(key-chord-mode 0)
(global-linum-mode 1)

;;;; Helm
; helm-rb
(setq helm-rb-get-methods-path "/Users/snakehsu/.emacs.d/get_methods.rb")

;;;; Org-mode
(require 'org)

(setq org-mobile-directory "/ssh:dev.idenizen.net#2121:/home/snakehsu/mobileorg")
(setq org-agenda-files (list "~/org/tasks.org"))

; capture settings
(setq org-default-notes-file (concat org-directory "/tasks.org"))
; capture templates
(setq org-capture-templates
      '(("o" "Office Todo" entry (file+headline org-default-notes-file "Office INBOX")
         "* TODO %?\n %i\n %a")
        ("h" "Home Todo" entry (file+headline org-default-notes-file "Home INBOX")
         "* TODO %?\n %i\n %a")
        ("b" "Bugs" entry (file+headline org-default-notes-file "Bugs")
         "* TODO %?\n %i\n %F")
        ("j" "Journal" entry (file+headline (concat org-directory "/journal.org") "STREAM")
         "* %? %U %^g\n %i\n")
        ("m" "Diary" entry (file+headline org-default-notes-file "Diary")
         "* %u \n** %i%(org-install-series-time-stamp 9 18)")))
; subscribe to RSS
(setq org-feed-alist
      '(("Instapaper"
         "http://www.instapaper.com/rss/303832/bf24ZocjTFsYE7Jn33qROfsLM"
         "~/org/tasks.org" "Read later")))
; refile settings
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
(setq org-refile-use-outline-path 'full-file-path)
(setq org-outline-path-complete-in-steps 'nil)

;;;; yasnippet
(yas-global-mode 1)

;;;; Hooks and mode-based keybindings
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)
(add-hook 'org-mode-hook (lambda () (linum-mode 0)))
(add-hook 'org-mode-hook (lambda () (whitespace-mode 0)))
(add-hook 'org-mode-hook (lambda () (define-key evil-normal-state-map
                                      (kbd "TAB") 'org-cycle)))
(add-hook 'org-mode-hook (lambda () (git-auto-commit-mode 1)))
(add-hook 'markdown-mode-hook
          (lambda () (local-set-key (kbd "C-c C-e") #'preview-in-marked-app)))
(add-hook 'evil-normal-state-entry-hook 'im-use-dvorak)
(add-hook 'evil-insert-state-entry-hook 'im-use-prev)
(add-hook 'evil-insert-state-exit-hook 'im-remember)
(add-hook 'evil-replace-state-entry-hook 'im-use-prev)
(add-hook 'evil-replace-state-exit-hook 'im-remember)
(add-hook 'evil-emacs-state-entry-hook 'im-use-dvorak)

;;;; Keybindings
(define-key global-map "\C-cc" 'org-capture)
(define-key evil-insert-state-map (kbd "RET") 'evil-ret-and-indent)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
