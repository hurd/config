;; -*- Mode: Emacs-Lisp -*-
;; $Id$
;;
;; Carbon Emacs
;;

(when (display-graphic-p)
  ;; font
  ;;(require 'carbon-font)
  ;; transparency
  (setq frame-alpha-lower-limit 20)
  (set-frame-parameter nil 'alpha '(95 95))
  ;; M-Space ==> just one space
  (global-set-key "\M- " 'just-one-space)
  ;(setq mac-command-key-is-meta nil)
  ;; use command, option keys as alt, meta keys
  (setq mac-command-modifier 'alt)
  (setq mac-control-modifier 'control)
  (setq mac-option-modifier 'meta)
  ;; home, end keys
  (define-key global-map [home] 'beginning-of-buffer)
  (define-key global-map [end] 'end-of-buffer)
  ;; mac-key-mode
  ;(require 'mac-key-mode)
  ;(mac-key-mode 1)
  ;; anti alias
  (setq mac-allow-anti-aliasing t)
)

;;
;; macosx terminal
;;
(defun mac-open-terminal ()
  (interactive)
  (let ((dir ""))
    (cond
     ((and (local-variable-p 'dired-directory) dired-directory)
      (setq dir dired-directory))
     ((stringp (buffer-file-name))
      (setq dir (file-name-directory (buffer-file-name))))
     )
    (do-applescript
     (format "
tell application \"Terminal\"
  activate
  try
    do script with command \"cd %s\"
  on error
    beep
  end try
end tell" dir))
))
