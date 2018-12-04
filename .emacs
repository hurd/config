;; -*- Mode: Emacs-Lisp -*-
;;
;; dot emacs for hurd (justhurd@gmail.com)
;;

;;
;; global settings
;;

;; provide a useful error trace if loading .emacs fails.
;;(setq max-specpdl-size 32000)
(setq debug-on-error t)

;;
;; package
;;
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives
               '(("gnu" . "http://elpa.gnu.org/packages/")
                 ("marmalade" . "http://marmalade-repo.org/packages/")
                 ("melpa" . "http://melpa.org/packages/")
                 )
               )
  )

;; abbrev
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

;; set my elisp path
(setq load-path (cons "~/.elisp" load-path))

;; turn off backup~
(setq make-backup-files nil)
;; stop creating #autosave# files
(setq auto-save-default nil)

;;
;; other settings
;;
(load "bindings")
(load "datetime")
(load "looknfeel")
(load "conventions")
(load "korean")
(when (eq system-type 'darwin)
  (load "macosx"))
(load "utils")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (hungry-delete web-mode python-mode auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
