;; -*- Mode: Emacs-Lisp -*-
;; $Id: .emacs 4 2011-05-11 05:20:21Z hurd $
;;
;; dot emacs for hurd (justhurd@gmail.com)
;;

;;
;; global settings
;;

;; provide a useful error trace if loading .emacs fails.
(setq debug-on-error t)

;; set my elisp path
(setq load-path (cons "~/.elisp" load-path))

(load "bindings")
(load "datetime")
(load "looknfeel")
(load "conventions")
(load "korean")
(when (eq system-type 'darwin)
  (load "macosx"))
(load "utils")

;; package
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t))
