;; -*- Mode: Emacs-Lisp -*-
;; $Id$
;;

;; tramp
(use-package tramp
  :defer t
  :config
  (setq tramp-persistency-file-name
        (concat temporary-file-directory "tramp-" (user-login-name))))

;; ido
(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-mode t))

;; reformat buffer
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f10] 'indent-buffer)

;; dos --> unix
(defun dos2unix ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\r" nil t)
      (replace-match ""))))

;; pbcopy & pbpaste
(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

(global-set-key (kbd "C-c c") 'pbcopy)
(global-set-key (kbd "C-c v") 'pbpaste)
(global-set-key (kbd "C-c x") 'pbcut)

;; google translater
(use-package google-translate
  :bind
  ("M-o t" . google-translate-at-point)
  ("M-o T" . google-translate-at-point-reverse)
  :custom
  (google-translate-default-source-language "ko")
  (google-translate-default-target-language "en"))
