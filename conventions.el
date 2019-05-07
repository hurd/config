;; -*- Mode: Emacs-Lisp -*-
;; $Id$
;;
;; code conventions & related tools
;;

;; make text mode the default mode for new buffer
(setq default-major-mode 'text-mode)
;; turn on auto fill mode automatically in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; default tabs mode to nil
(setq-default indent-tabs-mode nil)
;; UNIX mode for EOL char
(setq inhibit-eol-conversion t)
;; delete-trailing-whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; emacs24 stuff
(when (>= emacs-major-version 24)
  (setq delete-active-region nil))
;; hungry-delete
(require 'hungry-delete)
(global-hungry-delete-mode)

(add-hook 'prog-mode-hook
          '(lambda()
             (setq subword-mode t)
             ;(setq show-trailing-whitespace t)
             (add-hook 'before-save-hook
                       ;; delete trailing whitespace on save
                       'delete-trailing-whitespace nil t)
             (when (featurep 'dtrt-indent)
               (dtrt-indent-mode t))))

;;
;; Interactively Do Things (ido-mode)
;;
(require 'ido)
(ido-mode t)

;;
;; auto-complete (http://auto-complete.org)
;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.elisp/ac-dict")
(ac-config-default)

;;
;; C/C++ style
;;
(setq c-default-style "Stroustrup")
(setq c++-default-style "Stroustrup")

(add-hook 'c-mode-common-hook
          '(lambda()
             (c-toggle-auto-hungry-state 1)
             (local-set-key "\C-m" 'newline-and-indent)
             (setq c-basic-offset 4)
             (setq tab-width 4)
             (setq indent-tabs-mode nil)
             (when (featurep 'dtrt-indent)
               (dtrt-indent-mode t))))

;; use gdb-script-mode for files ending in .gdb
(setq auto-mode-alist
      (cons '("\\.gdb$" . gdb-script-mode) auto-mode-alist))

;; compile
(setq compile-command "g++ -g -W -Wall -Wcast-qual -Wshadow -D_DEBUG")
;(setq compile-command "gmake -f Makefile")
(add-to-list 'auto-mode-alist '("\\.mq4\\'" . c++-mode))

;;
;; php-mode. http://www.ontosys.com/reports/PHP.html
;;
;(autoload 'php-mode "php-mode" "PHP editing mode" t)
;(add-to-list 'auto-mode-alist '("\\.html$" . php-mode))
;(add-to-list 'auto-mode-alist '("\\.shtml$" . php-mode))
;(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;(add-to-list 'auto-mode-alist '("\\.php3$" . php-mode))
;(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;;
;; web-mode (http://web-mode.org)
;;
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustanche\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.dhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;
;; python-mode
;;
(setq py-install-directory "~/.elisp/python-mode")
(add-to-list 'load-path py-install-directory)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq interpreter-mode-alist (cons'("python" . python-mode)
        interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
;; fix backspace problem
(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map (kbd "DEL") 'py-electric-backspace)))

;;
;; d-mode
;;
(autoload 'd-mode "d-mode" () t)
(add-to-list 'auto-mode-alist '("\\.d\\'" . d-mode))
(add-hook 'd-mode-hook 'imenu-add-menubar-index)
(add-hook 'd-mode-hook 'font-lock-mode)

;;
;; vc-svn mode for SVN
;;
(require 'cl)
(load-library "vc-svn")

;;
;; css-mode
;;
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

;;
;; js2-mode
;;
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)


;;
;; ruby-mode
;;
(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'"
               . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'"
               . ruby-mode))

;;
;; textile mode
;; https://github.com/juba/textile-mode
;;
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;;
;; rust-mode
;;
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;
;; Auto insert comment
;;
(global-unset-key "\C-t")
(global-set-key "\C-t\C-h" 'insert-function-header)

(defun insert-function-header () (interactive)
  (insert "    //\n")
  (insert "    //\n")
  ;(insert " * @author: hurd\n")
  (insert "    // @param:\n")
  (insert "    // @return:\n")
  (insert "    //"))

(global-set-key "\C-t\C-g" 'insert-file-header)
(defun insert-file-header () (interactive)
  (insert "/**\n")
  (insert " * -*- Mode: C; c-basic-indent: 4; indent-tabs-mode: nil -*-\n")
  (insert " *\n")
  (insert " * Copyright (c) 2017-2018, Crefun inc., All rights reserved.\n")
  (insert " * $Id$\n")
  ;(insert " * Author: Choe, Youngbong (justhurd@gmail.com)\n")
  (insert " *\n")
  (insert " * Desc: \n")
  (insert " *\n")
  (insert " */\n"))
