;;; behaviour.el --- customize the overall behaviour

(server-start)
;; Evil-mode
(require 'evil)
(evil-mode 1)
; disable key-chord mode
(key-chord-mode 0)
(global-linum-mode 1)
