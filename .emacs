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
  (setq package-list '(auto-complete hungry-delete python-mode web-mode))
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/"))
  (package-initialize)
  ;; fetch all the packages
  (unless package-archive-contents
    (package-refresh-contents))
  ;; install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))
  )

;; install
(unless (package-installed-p 'hungry-delete)
  (package-refresh-contents)
  (package-install 'hungry-delete))
(unless (package-installed-p 'auto-complete)
  (package-refresh-contents)
  (package-install 'auto-complete))
(unless (package-installed-p 'web-mode)
  (package-refresh-contents)
  (package-install 'web-mode))
(unless (package-installed-p 'python-mode)
  (package-refresh-contents)
  (package-install 'python-mode))
(unless (package-installed-p 'd-mode)
  (package-refresh-contents)
  (package-install 'd-mode))
(unless (package-installed-p 'cl)
  (package-refresh-contents)
  (package-install 'cl))
(unless (package-installed-p 'ruby-mode)
  (package-refresh-contents)
  (package-install 'ruby-mode))
(unless (package-installed-p 'textile-mode)
  (package-refresh-contents)
  (package-install 'textile-mode))

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
