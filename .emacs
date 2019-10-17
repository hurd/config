;; -*- Mode: Emacs-Lisp -*-
;;
;; dot emacs for hurd (justhurd@gmail.com)
;;

;;
;; global settings
`;;

;; provide a useful error trace if loading .emacs fails.
;;(setq max-specpdl-size 32000)
(setq debug-on-error t)

;;
;; package
;;
(require 'package)
(setq package-archives '(
                         ("elpa" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         )
      )
(package-initialize)

;; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; install other packages
(unless (package-installed-p 'ibuffer-sidebar)
  (package-refresh-contents)
  (package-install 'ibuffer-sidebar))
(unless (package-installed-p 'python-mode)
  (package-refresh-contents)
  (package-install 'python-mode))
(unless (package-installed-p 'smart-hungry-delete)
  (package-refresh-contents)
  (package-install 'smart-hungry-delete))
;;(unless (package-installed-p 'auto-complete)
;;  (package-refresh-contents)
;;  (package-install 'auto-complete))
(unless (package-installed-p 'web-mode)
  (package-refresh-contents)
  (package-install 'web-mode))
(unless (package-installed-p 'cl)
  (package-refresh-contents)
  (package-install 'cl))
(unless (package-installed-p 'ruby-mode)
  (package-refresh-contents)
  (package-install 'ruby-mode))

;; abbrev
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

;; set my elisp path
(setq load-path (cons "~/.elisp" load-path))

;; turn off backup~
(setq make-backup-files nil)
;; stop creating #autosave# files
(setq auto-save-default nil)

;; tramp
(use-package tramp
  :defer t
  :config
  (setq tramp-persistency-file-name
        (concat temporary-file-directory "tramp-" (user-login-name))))

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
 '(column-number-mode t)
 '(display-time-mode t)
 '(package-selected-packages
   (quote
    (ibuffer-sidebar monokai-theme web-mode vscode-icon use-package python-mode hungry-delete dired-sidebar auto-complete)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
