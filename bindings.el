;; -*- Mode: Emacs-Lisp -*-
;; $Id$
;;
;; key bindings
;;

;(global-set-key [delete] 'delete-char)
(global-set-key (kbd "M-g g") 'goto-line)
(global-set-key (kbd "C-h") 'backward-delete-char)
;; reload .emacs
(global-set-key (kbd "M-r")
                '(lambda () (interactive) (load-file "~/.emacs")))

;; disable ctrl Z
(global-unset-key "\^z")
;; disable that minize window thing
(global-unset-key "\C-x\C-z")

;; You know, like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; ;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; ;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

;; ;; Indentation help
(global-set-key (kbd "C-x ^") 'join-line)
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

;; ;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; ;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; ;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;; ;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-x C-h") 'view-url)

;; ;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

;; ;; cycle through buffers
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; ;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; ;; swap windows
(global-set-key (kbd "C-c s") 'swap-windows)

;; ;; duplicate the current line or region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; ;; rename buffer & visited file
(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

;; ;; toggle menu-bar visibility
(global-set-key (kbd "<f12>")
                (lambda () (interactive) (menu-bar-mode)))
