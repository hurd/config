;; -*- Mode: Emacs-Lisp -*-
;; $Id$
;;
;; key bindings
;;

;(global-set-key [delete] 'delete-char)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\^h" 'backward-delete-char)
;; reload .emacs
(global-set-key "\M-r"
                '(lambda () (interactive) (load-file "~/.emacs")))

;; disable ctrl Z
(global-unset-key "\^z")
;; disable that minize window thing
(global-unset-key "\C-x\C-z")
