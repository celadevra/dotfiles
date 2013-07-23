;;; Look.el --- customize the interface

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

; Add space between linum and text
; from http://www.emacswiki.org/emacs/LineNumbers#toc7
(defvar my-linum-format-string "%4d")
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)
(defun my-linum-get-format-string ()
  (let* ((width (length (number-to-string
                         (count-lines (point-min) (point-max)))))
         (format (concat "%" (number-to-string width) "d ")))
    (setq my-linum-format-string format)))
(setq linum-format 'my-linum-format)
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

(provide 'look)

;; look.el ends here
