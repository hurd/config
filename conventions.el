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
;(require 'hungry-delete)
;(global-hungry-delete-mode)

;; smart-hungry-delete
(use-package smart-hungry-delete
  :ensure t
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks
  :config
  (smart-hungry-delete-add-default-hooks))

(add-hook 'prog-mode-hook
          '(lambda()
             (setq subword-mode t)
             ;(setq show-trailing-whitespace t)
             (add-hook 'before-save-hook
                       ;; delete trailing whitespace on save
                       'delete-trailing-whitespace nil t)
             (when (featurep 'dtrt-indent)
               (dtrt-indent-mode t))))
;; isearch
(use-package isearch
  :bind (:map isearch-mode-map
              ("C-<return>" . isearch-done-opposite)
              ("M-i" . helm-swoop-from-isearch))
  :init (defun isearch-done-opposite (&optional nopush edit)
          "End current search in the opposite side of the match."
          (interactive)
          (funcall #'isearch-done nopush edit)
          (when isearch-other-end (goto-char isearch-other-end))))

;; Interactively Do Things (ido-mode)
(use-package ido
  :ensure t
  :init (ido-mode t))

;;
;; auto-complete (http://auto-complete.org)
;;
;;(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories "~/.elisp/ac-dict")
;;(ac-config-default)

(use-package flycheck
  :ensure t
  :if (display-graphic-p)
  :hook ((c++-mode typescript-mode racer-mode) . flycheck-mode))

(use-package flycheck-rust
  :ensure t
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package rust-mode
  :ensure t
  :config (setq rust-format-on-save t
                rust-match-angle-brackets nil))
;; company mode
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

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

;; web-mode (http://web-mode.org)
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.dhtml\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight nil
        )
  (add-hook 'web-mode-hook
            (defun setup/tsx ()
              (setq flycheck-checker 'tsx-tide)
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (tide-setup)
                (tide-hl-identifier-mode)
                (eldoc-mode)
                (flycheck-mode)
                (add-hook 'before-save-hook 'tide-format-before-save)))))

;; python-mode
(setq py-install-directory "~/.elisp/python-mode")
(add-to-list 'load-path py-install-directory)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq interpreter-mode-alist (cons'("python" . python-mode)
        interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
;; fix backspace problem
(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map (kbd "DEL") 'py-electric-backspace)))

;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;;
;; css-mode
;;
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

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
