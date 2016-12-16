;;; celadevra.el -- custom functions for using Emacs

;;; Commentary:
;;;   This package provides a few functions to facilitate using Emacs.

;;; Code:

(defvar org-journal-dir "~/org/journal/"
  "Location of journal files.  The trailing slash is important.")

(defun xhy/get-journal-name ()
  "Obtain journal name from current date."
  (let ((year-name (format-time-string "%Y")))
    (expand-file-name (concat org-journal-dir year-name ".org"))))

(provide 'celadevra)
;;; celadevra.el ends here
