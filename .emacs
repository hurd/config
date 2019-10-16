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
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(unless (package-installed-p 'python-mode)
  (package-refresh-contents)
  (Package-install 'python-mode))
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
(unless (package-installed-p 'cl)
  (package-refresh-contents)
  (package-install 'cl))
(unless (package-installed-p 'ruby-mode)
  (package-refresh-contents)
  (package-install 'ruby-mode))

(when (display-graphic-p)
; VsCode Icons for emacs
  (use-package vscode-icon
    :ensure t
    :commands (vscode-icon-for-file)))

;
;  dired-sidebar
;
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-subtree-line-prifix "__")
  (when (display-graphic-p)
    (setq dired-sidebar-theme 'vscode))
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

;;
;;

;; abbrev
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

;; set my elisp path
(setq load-path (cons "~/.elisp" load-path))

;; turn off backup~
(setq make-backup-files nil)
;; stop creating #autosave# files
(setq auto-save-default nil)

;;
;; tramp
;;
(use-package tramp
  :defer t
  :custom
  (setq tramp-default-method "ssh")
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
