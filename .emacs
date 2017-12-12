;; -*- Mode: Emacs-Lisp -*-
;; $Id: .emacs 4 2011-05-11 05:20:21Z hurd $
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
