;; -*- Mode: Emacs-Lisp -*-
;;
;; dot emacs for hurd (justhurd@gmail.com)
;;

;; provide a useful error trace if loading .emacs fails.
;;(setq max-specpdl-size 32000)
(setq debug-on-error t)

;;
;; el-get
;; cd ~/.emacs.d && git clone https://github.com/dimitri/el-get.git
;;
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; package
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
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'el-get)
    (package-install 'use-package)
    (package-install 'diminish)
    (package-install 'ibuffer-sidebar)
    (package-install 'ivy)
    (package-install 'ag)
    (package-install 'dumb-jump)
    (package-install 'python-mode)
    (package-install 'smart-hungry-delete)
    (package-install 'web-mode)
    (package-install 'cl)
    (package-install 'ruby-mode)
    (package-install 'ido)
    (package-install 'popwin)
    (package-install 'guide-key)
    (package-install 'markdown-mode)
    (package-install 'bind-key))

  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (require 'use-package))

(use-package diminish)
(use-package bind-key)

;; abbrev
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

;; set my elisp path
(setq load-path (cons "~/.elisp" load-path))

;; turn off backup~
(setq make-backup-files nil)
;; stop creating #autosave# files
(setq auto-save-default nil)

;; loading related
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(defun display-startup-echo-area-message ()
  (message ""))

; Deleting files go to OS's trash folder
(setq delete-by-moving-to-trash t)

;; keep cursor at end of lines
(setq track-eol t)
;; to be required by track-eol
(setq line-move-visual nil)
;; kill line including '\n'
(setq-default kill-whole-line t)
(defalias 'yes-or-no-p #'y-or-n-p)

(when (functionp 'mac-auto-ascii-mode)
  (mac-auto-ascii-mode 1))

;;
;; other settings
;;
(load "bindings")
(load "datetime")
(load "looknfeel")
(load "conventions")
;(load "korean")
(when (eq system-type 'darwin)
  (load "macosx"))
(load "utils")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(dashboard-center-content t)
 '(dashboard-items (quote ((recents . 15) (projects . 5) (bookmarks . 5))) t)
 '(dashboard-startup-banner 4 t)
 '(display-time-mode t)
 '(global-display-line-numbers-mode t)
 '(hydra-posframe-parameters (quote ((left-fringe . 5) (right-fringe . 5))) t)
 '(package-selected-packages
   (quote
    (smartparens ibuffer-sidebar monokai-theme web-mode vscode-icon use-package python-mode hungry-delete dired-sidebar auto-complete)))
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-heading ((t (:foreground "#f1fa8c" :weight bold))))
 '(hydra-posframe-border-face ((t (:background "#6272a4"))))
 '(show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))))
