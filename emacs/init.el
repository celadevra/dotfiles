;;; init.el -- 徐浩洋的 Emacs 设置  -*- fill-column: 80; outline-regexp: ";;;\\(;* [^ \t\n]\\)"; -*-
;;; Commentary:
;; 一套保存在 SDF MetaArray 上的 Emacs 设置，争取实现到处可用。

;;; Code:

;;; 网络相关

(setenv "HTTP_PROXY" "http://127.0.0.1:8118")
(setenv "HTTPS_PROXY" "http://127.0.0.1:8118")

(setq user-mail-address "celadevra@sdf.org")
(setq user-full-name "徐浩洋")

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'message-smtpmail-send-it
      smtpmail-stream-type 'ssl
      smtpmail-smtp-service 465
      mail-specify-envelope-from t
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

;;; 区域和语言设置

(set-language-environment 'UTF-8)

(setq sentence-end-double-space nil)
(setq sentence-end nil)

;;; 路径设置

(if (eq window-system 'w32)
    (progn
      (add-to-list 'exec-path "C:/ProgramData/chocolatey/bin")
      (add-to-list 'exec-path "C:/Program Files/Git/bin")
      (add-to-list 'exec-path "c:/Program Files/Racket")
      (add-to-list 'exec-path "c:/Program Files/R/R-4.1.0/bin")
      (setenv "PATH" "c:/Program Files/R/R-4.1.0/bin;c:/Program Files/Racket;c:/Program Files/Git/bin;/C:/ProgramData/chocolatey/bin;C:/Program Files/PowerShell/7-preview;C:/Python39/Scripts/;C:/Python39/;C:/Program Files (x86)/Intel/Intel(R) Management Engine Components/iCLS/;C:/Program Files/Intel/Intel(R) Management Engine Components/iCLS/;C:/Windows/system32;C:/Windows;C:/Windows/System32/Wbem;C:/Windows/System32/WindowsPowerShell/v1.0/;C:/Windows/System32/OpenSSH/;C:/Program Files (x86)/Intel/Intel(R) Management Engine Components/DAL;C:/Program Files/Intel/Intel(R) Management Engine Components/DAL;C:/ProgramData/chocolatey/bin;C:/Program Files/VSCodium/bin;C:/Program Files/PowerShell/7-preview/preview;C:/ProgramData/chocolatey/lib/mpv.install/tools;;C:/Program Files/Docker/Docker/resources/bin;C:/ProgramData/DockerDesktop/version-bin;C:/Program Files/nodejs/;C:/oracle/11.2.0/bin;C:/msys64/usr/bin;C:/tools/neovim/Neovim/bin;C:/Users/CiticBooks/AppData/Local/Pandoc/;C:/Users/CiticBooks/AppData/Roaming/npm"))
  (progn
    (add-to-list 'exec-path "/usr/local/bin")
    (setenv "PATH" "$HOME/bin:$HOME/.local/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin")))

;;; 基础行为

(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq delete-old-versions t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; 字体设置

(if (display-graphic-p)
    (progn ;; set English font
      (set-face-attribute 'default nil :font "IBM Plex Mono-11")
      ;; Chinese font
      (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
  			charset (font-spec :family "Noto Serif CJK SC"
  					   :size 11)))
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
      (if (or (eq window-system 'w32) (eq window-system 'x))
  	(setq face-font-rescale-alist '(("Noto Serif CJK SC" . 2.0)))
      (setq face-font-rescale-alist '(("Noto Serif CJK SC" . 1.2))))))

;;; straight.el 包管理器

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; 安装 general.el 按键配置扩展

(straight-use-package 'general)
(global-unset-key (kbd "C-z"))
(general-create-definer my-leader-def
  :prefix "C-z")

(straight-use-package 'which-key)
(which-key-mode)

;;; 补全系统

(straight-use-package 'swiper)
(straight-use-package 'ivy)
(straight-use-package 'counsel)
(ivy-mode 1)
(counsel-mode 1)
(general-def "M-x" 'counsel-M-x)
(general-def "C-s" 'swiper)

(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;; 编程辅助

;;;; Smartparens

(straight-use-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode)

(general-define-key
 :keymaps 'smartparens-strict-mode-map
 "C-M-a" 'sp-beginning-of-sexp
 "C-M-e" 'sp-end-of-sexp
 "M-n" 'sp-down-sexp
 "M-p" 'sp-up-sexp
 "M-<up>" 'sp-backward-up-sexp
 "M-<down>" 'sp-backward-down-sexp
 "C-M-f" 'sp-forward-sexp
 "C-M-b" 'sp-backward-sexp
 "C-M-p" 'sp-previous-sexp
 "C-M-n" 'sp-next-sexp
 "C-S-b" 'sp-backward-symbol
 "C-S-f" 'sp-forward-symbol
 "M-[" 'sp-backward-unwrap-sexp
 "M-]" 'sp-unwrap-sexp
 "C-<right>" 'sp-forward-slurp-sexp
 "M-<right>" 'sp-forward-barf-sexp
 "C-<left>" 'sp-backward-slurp-sexp
 "M-<left>" 'sp-backward-barf-sexp
 "C-M-t" 'sp-transpose-sexp
 "C-M-k" 'sp-kill-sexp
 "C-k" 'sp-kill-hybrid-sexp
 "M-k" 'sp-backward-kill-sexp)

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)

;;;; Others

(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(straight-use-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(straight-use-package 'git-auto-commit-mode)

(setq-default gac-automatically-push-p t
  	    gac-debounce-interval 30)

(straight-use-package 'diminish)
(diminish 'which-key-mode)
(diminish 'counsel-mode)
(diminish 'ivy-mode)
(diminish 'auto-fill-function "⏎|")
(diminish 'company-mode "█_")
(diminish 'olivetti-mode "⇉⇇")

;;;; Magit

(straight-use-package 'magit)

(my-leader-def
  "gg" 'magit-status)

;;;; Projectile

(straight-use-package 'projectile)
(straight-use-package 'ag)

(my-leader-def
  "/" 'projectile-ag
  "p" 'projectile-command-map)

;;; Org-mode

(straight-use-package 'org)
(require 'org)

(setq org-agenda-files '("~/zettelkasten"))
(setq org-agenda-start-on-weekday nil)

(add-to-list 'org-export-backends 'org)

;;;; 日程

(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs"
       ((alltodo "" nil))
       nil)
      ("b" "Next to do" todo "NEXT" nil)))

;;;; 分拣

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))

;;;; 标签与类别

(setq org-tag-alist
      '((:startgroup . nil)
	("project" . ?p) ("milestone" . ?l) ("issue" . ?i)
	(:endgroup . nil)
	("documentation" . ?d)))

(add-to-list 'org-modules 'org-id)

(setq org-id-link-to-org-use-id t)

;;;; 捕捉

;; TODO: 换用新的模板；捕捉到 Gemini/Gopher Garden 中

(setq org-capture-templates
      '(("t" "New task" entry (file+olp "~/zettelkasten/tasks.org" "捕捉")
	 "* NEXT %? %^g\n")
	("f" "Fleeting notes" entry (function xhy/find-org-capture-target)
	 "* %^{Topic? } %^g:FLAGGED:\n  CREATED: %t\n\n" :time-prompt t)
	("g" "Gemini Entry" plain (function xhy/find-gemini-capture-target)
	 "###%(number-to-string (with-temp-buffer (insert-file-contents (buffer-file-name (plist-get org-capture-plist :buffer))) (1+ (count-matches \"^###\" (point-min) (point-max)))))\n\n%?")))

(defun xhy/find-org-capture-target ()
  "Find org capture target by user's input.

The user gives a date, or use the default date -- today. The
target is a file named YYYYMM.org, and captured entry go to a
week-based date tree."
  ()
  (let* ((prompt-time (plist-get org-capture-plist :time-prompt))
	 (default-time (if prompt-time (org-capture-put :default-time (org-read-date nil t))
  		       (org-capture-put :default-time (current-time))))
	 (month (format-time-string "%Y-%m"
  				    (plist-get org-capture-plist :default-time)))
	 (week (concat "Week " (format-time-string "%U"
  						   (plist-get org-capture-plist :default-time))))
	 (date (concat "[" (format-time-string "%Y-%m-%d %a"
  					       (plist-get org-capture-plist :default-time)) "]"))
	 (target-file (concat "~/zettelkasten/" month ".org"))
	 (file-exists (file-exists-p target-file)))
    (find-file target-file)
    (unless (ignore-errors
  	    (org-find-exact-headline-in-buffer week))
      (goto-char (point-min))
      (org-insert-heading nil nil t)
      (insert week))
    (goto-char (org-find-exact-headline-in-buffer week))
    (unless (ignore-errors (org-find-exact-headline-in-buffer date))
      (goto-char (org-find-exact-headline-in-buffer week))
      (end-of-line)
      (org-insert-subheading t)
      (insert date))
    (goto-char (org-find-exact-headline-in-buffer date))))

(defun xhy/get-current-heading-content ()
  "Copy current heading's content then extract the string from kill ring."
  ()
  (org-back-to-heading)
  (org-copy-subtree)
  (substring-no-properties (car kill-ring)))

(add-hook 'org-capture-before-finalize-hook 'org-id-get-create)
;;(add-hook 'org-capture-after-finalize-hook 'xhy/checkin-zettel)

(setq org-todo-keywords '((sequence "NEW" "|" "NOTE")
  			(sequence "NEXT" "TODAY(t!)" "|" "DONE(d!)" "CANCELLED(c!)")))

;;;; 按键配置

(my-leader-def
  "oa"  'org-agenda
  "oc"  'org-capture
  "ol"  'org-store-link
  "oL"  'org-insert-link
  "ot"  'org-insert-structure-template)

(add-hook 'org-mode-hook 'auto-fill-mode)

;;;; 输出设置

(defadvice org-html-paragraph (before org-html-paragraph-advice
  				    (paragraph contents info) activate)
  "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
  (let* ((origin-contents (ad-get-arg 1))
       (fix-regexp "[[:multibyte:]]")
       (fixed-contents
  	(replace-regexp-in-string
  	 (concat
  	  "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))

    (ad-set-arg 1 fixed-contents)))

;;;; Babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (js . t)
   (ruby . t)
   (R . t)))

(setq org-babel-js-function-wrapper
      "process.stdout.write(require('util').inspect(function(){\n%s\n}()));")

(add-to-list 'org-src-lang-modes
  	   '("js" . js2))

;;;; Zettelkasten

(defun xhy/zettel-search-back-link ()
  "Search for zettels that link to this zettel, and show them in agenda view."
  (interactive)
  (let ((this-id (org-id-get)))
    (if this-id
	(org-search-view nil (concat "[id:" this-id))
      (message "This zettel has no ID yet."))))

(my-leader-def
  "or" 'xhy/zettel-search-back-link)

(defun xhy/get-headings ()
  "Get headings and ids in each org entry with TODO keywords.
  Store them in an alist like this:
  '((heading1 . id1)
    (heading2 . id2)
    ...)
  and return the alist."
  ()
  ;; make file list based on org-agenda-files
  (let ((f-list ())
      (headings ())
      (ids ())
      (headings-alist))
    (dolist (d-or-f-name org-agenda-files)
      (if (directory-name-p d-or-f-name)
  	(setq f-list (append (directory-files d-or-f-name t "^[0-9]\\{4\\}-[0-9]\\{2\\}\\.org$") f-list))
      (push d-or-f-name f-list)))
    ;; one file at a time, get every heading and their ID
    (org-map-entries
     (lambda () (push (org-get-heading) headings)
       (push (org-id-get-create) ids)
       (while (and headings ids) ; don't have zip, so a hack
       (push (cons (pop headings) (pop ids)) headings-alist)))
     "TODO={.+}"
     f-list)
    headings-alist))

(defun xhy/insert-link-from-ivy-selection (head-id-cons)
  "Insert link to `HEAD-ID-CONS' at current region.

Or if no region, insert a link to EID with the description part as HEADING."
  (interactive)
  (if (use-region-p)
      (org-insert-link nil (concat "id:" (cdr head-id-cons)))
    (org-insert-link nil (concat "id:" (cdr head-id-cons)) (car head-id-cons))))

(defun xhy/zettel-create-link-with-search (&optional beg end)
  "With a selected region, read from user input for the search
  term. Text in the region will be used as default. Use it to
  narrow down the candidate list. Once the user select an
  entry, insert link at the region."
  (interactive "r")
  (let ((initial-input (if (use-region-p)
  			 (buffer-substring beg end)
  		       (thing-at-point 'symbol t))))
    (ivy-read "Search string: "     ; PROMPT
  	    (xhy/get-headings)    ; COLLECTION
  	    :predicate nil
  	    :require-match t
  	    :initial-input initial-input
  	    :action 'xhy/insert-link-from-ivy-selection)))

(my-leader-def
  "ok" 'xhy/zettel-create-link-with-search
  "oo" 'counsel-org-goto
  "oO" 'counsel-org-goto-all)

;;;; org-ref

(straight-use-package 'org-ref)

(setq org-ref-default-bibliography '("~/zettelkasten/references.bib"))
(setq reftex-default-bibliography '("~/zettelkasten/references.bib"))

(setq bibtex-completion-bibliography "~/zettelkasten/references.bib")

;;; Hyperbole

(straight-use-package
 '(hyperbole
   :files ("*.el" ("kotl" "kotl/*.el") "man/*.info" "man/*.texi")
   :host github :repo "rswgnu/hyperbole"))
(my-leader-def
  "h" 'hyperbole)
(setq hbmap:dir-user "~/zettelkasten/.hyperb")

;;; Markdown

(straight-use-package 'markdown-mode)

(setq auto-mode-alist
      (append auto-mode-alist '(("\\.md\\'" . markdown-mode)
  			      ("README\\.md\\'" . gfm-mode)
  			      ("\\.pmd\\'" . markdown-mode))))
;(add-hook 'markdown-mode-hook 'olivetti-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(setq markdown-command "pandoc")

;;; JS

(straight-use-package 'json-mode)

(straight-use-package 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode))

;;; Python

(straight-use-package 'elpy)

;;; R

(straight-use-package 'ess)
(load "ess-autoloads")

;;; Ruby

(straight-use-package 'inf-ruby)

(straight-use-package 'chruby)

(straight-use-package 'robe)

;;; Racket

(straight-use-package 'racket-mode)

(require 'racket-xp)
(add-hook 'racket-mode-hook #'racket-xp-mode)

(straight-use-package 'pollen-mode)
(straight-use-package 'company-pollen)
(require 'company-pollen)

;;; Gemini and Gopher

(straight-use-package '(gemini-mode
			:repo "http://git.carcosa.net/jmcbray/gemini.el.git"))
(straight-use-package 'elpher)

(my-leader-def
  "ag" 'elpher)

(defvar xhy-v/gopher-dir "~/Sites/gopher-sdf"
  "Where the gopher root is stored on the local machine.")

(defvar xhy-v/phlog-dir (concat xhy-v/gopher-dir "/phlog")
  "Location of the phlog directory.

By default, it is `phlog' under the `xhy-v/gopher-dir'.")

(defvar xhy-v/gemini-dir "~/Sites/gemini"
  "Where the gemini dir resides.")

(defun xhy/phlog--update-gophermap (year month)
  "Update gophermap for phlogs at path: `xhy-v/phlog-dir'/YEAR/MONTH.

First, update the month level gophermap. This is done by sorting
the files by name, then reading each file's first line. Also, we
give the gophermap a header. Content generating happens in a temp
buffer then written to the file.

Then the year level map is updated similarly."
  ()
  (let ((year-map-path (concat xhy-v/phlog-dir "/" year))
	(month-map-path (concat xhy-v/phlog-dir "/" year "/" month)))
    ;; month map
    (with-temp-buffer (insert "!" year "/" month "\n") ; header
		      (let ((file-list
			     (reverse (directory-files month-map-path t "[0-9]+")))
			    (file-name-list
			     (reverse (directory-files month-map-path nil "[0-9]+"))))
			(setq first-lines (mapcar (lambda (file-path)
						    (with-temp-buffer
						      (insert-file-contents file-path)
						      (goto-char (point-min))
						      (string-trim
						       (thing-at-point 'line))))
						  file-list))
			(setq lines+files (seq-mapn #'cons first-lines file-name-list))
			(mapc (lambda (elm) (insert "0" (car elm) "\t" (cdr elm) "\n"))
			      lines+files))
		      (write-file (concat month-map-path "/" "gophermap")))
    ;; year map
    (with-temp-buffer (insert "!" year "\n")
		      (let ((file-list (reverse
					(directory-files year-map-path nil "[0-9]+")))
			    (month-list '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
					  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
			(mapc (lambda (elm)
				(insert "1"
					(elt month-list (1- (string-to-number elm)))
					"\t" elm "/\n"))
			      file-list)
			(write-file (concat year-map-path "/" "gophermap"))))))

(defun xhy/phlog ()
  "Create a phlog entry and update gophermaps.

The function asks the user for the date and the title of the
entry, then create a directory hierarchy in the form of
YYYY/MM/DD in case any of the level is not available. It then
finds the file YYYY-MM-DD.txt and let the user edit it. The first
line of the file would be the title given by the user or just the
date, followed by 50 \"=\" and an empty line.

Also, it updates the gophermap at the YYYY level by adding the
directory link to the MM dir in the map.

The gophermap at the MM level is updated with the title given by
the user."
  (interactive)
  (let* ((date (org-read-date nil nil nil "Phlog entry for which day? "))
	 (date-split (split-string date "-")) ; ("YYYY" "MM" "DD")
	 (year (car date-split))
	 (month (car (cdr date-split)))
	 (day (cdr (cdr date-split)))
	 (title (concat "[" date "] "
			(read-string "Title for this entry? " nil nil )))
	 (file-name (concat date ".txt"))
	 (full-file-path (concat xhy-v/phlog-dir "/" year "/"
			       month "/" file-name)))
    (if (file-exists-p full-file-path)
	;; file exists, the new entry is just a section in the log
	(progn (find-file full-file-path)
	       (goto-char (point-max))
	       (insert "\n" title "\n")
	       (insert-char ?- 50)
	       (insert "\n") ; wait for input
	       )
      ;; more complicated situation, need to update gophermap
      (let ((path (concat xhy-v/phlog-dir "/" year "/" month)))
	(make-directory (concat xhy-v/phlog-dir "/" year "/" month) t)
        (find-file full-file-path)
	(insert title "\n")
	(insert-char ?= 50)
	(insert "\n")
	(save-buffer)
	(xhy/phlog--update-gophermap year month)))))

(defun xhy/find-gemini-capture-target ()
  "Find the correct file to store the captured snippet.

This function creates a list of file names without the .gmi part
under the directory `xhy-v/gemini-dir', and use it as the base
for the ivy selection. If the file exists, it is used as the
target. If not, the file is created with the user input as its
name and title. Also, it calls `xhy/gemini-generate-index' to
update the index."
  ()
  (let* ((filelist (mapcar (lambda (x) (substring x 0 -4)) (directory-files xhy-v/gemini-dir nil ".gmi")))
	 (filename (ivy-read "Topic: " filelist))
	 (fullname (concat xhy-v/gemini-dir "/" filename ".gmi"))
	 (exist (file-exists-p fullname)))
    (if exist
	;; file exists
	(progn
	  (find-file fullname)
	  (goto-char (point-max))
	  (insert "\n\n"))
      ;; file don't exist yet
      (progn
	(with-temp-buffer
	  (insert "#")
	  (insert filename)
	  (insert "\n\n")
	  (write-file fullname t))
	(xhy/gemini-generate-index)
	(find-file fullname)
	(goto-char (point-max))))))

(defun xhy/gemini-generate-index ()
  "Generate the index gemini file from files in `xhy-v/gemini-dir'."
  (interactive)
  (let* ((filelist (directory-files xhy-v/gemini-dir nil "[^index].gmi"))
	 (topiclist (mapcar (lambda (x) (substring x 0 -4)) filelist))
	 (file-topic-pair '()))
    (while (and filelist topiclist)
      (push (cons (pop filelist) (pop topiclist)) file-topic-pair))
    (with-temp-buffer
      (insert "#吐槽大全\n\n")
      (mapc (lambda (x) (insert "=>" (car x) " " (cdr x) "\n")) file-topic-pair)
      (write-file (concat xhy-v/gemini-dir "/index.gmi")))))

;;; Elfeed

(straight-use-package 'elfeed)

(setq elfeed-curl-extra-arguments '("-x" "socks5://127.0.0.1:1080"))

(setq elfeed-feeds
      '(("http://www.clarkesworldmagazine.com/feed/rss/" fiction magazine)
	("https://www.lightspeedmagazine.com/rss-2/" magazine)
	("http://thedarkmagazine.com/rss" fiction magazine horror)
	("http://www.ttapress.com/interzone/feed/" magazine fiction)
	("http://www.beneath-ceaseless-skies.com/issues/feed/" magazine fiction)
	("https://uncannymagazine.com/feed/" magazine fiction)
	("https://www.nightmare-magazine.com/rss-2/" magazine fiction horror)
	("http://www.johnjosephadams.com/feed/" editor fiction)
	("https://mithilareview.com/feed/" magazine fiction review)
	("https://publicdomainreview.org/rss.xml" review)
	("https://vector-bsfa.com/feed/" review)
	("http://asianreviewofbooks.com/content/index.php/feed/" review)
	("https://aeon.co/feed.rss" nonfiction)
	("https://www.citylab.com/feeds/posts/" nonfiction)
	("http://www.4sbooks.com/feed" review)
	("https://the-rambling.com/feed/" nonfiction)))

(my-leader-def
  "ae" 'elfeed)
;;; 外观

(load-theme 'tsdh-light)

(custom-theme-set-faces
 'tsdh-light
 '(cursor ((t (:background "#F29112")))))

(setq custom-safe-themes '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default))

(straight-use-package 'smart-mode-line)

(setq sml/theme 'respectful)

(sml/setup)

;;; Custom Set Variables

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((comp))))
