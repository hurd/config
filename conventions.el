;; -*- Mode: Emacs-Lisp -*-
;; $Id$
;;
;; code conventions & related tools
;;

;; make text mode the default mode for new buffer
(setq default-major-mode 'text-mode)
;; turn on auto fill mode automatically in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; default tabs mode to nil ==> use space
(setq-default indent-tabs-mode nil)
;; UNIX mode for EOL char
(setq inhibit-eol-conversion t)
;; delete-trailing-whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; emacs24 stuff
(when (>= emacs-major-version 24)
  (setq delete-active-region nil))

;; fix trailing spaces
(use-package ws-butler
  :defer
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode))

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

;;
;; virtual environment: conda
;;
;; 1) conda create -n webdev python
;; 2) pip install sexpdata epc
;; 3) conda install jedi
;; 4) cd ~/.emacs.d/elpa/jedi-core* && python setup.py install
;; 5) conda activate base && pip install flake8 pylint
;;
(eval-after-load "company-jedi"
    '(setq jedi:server-command (list "~/miniconda3/envs/webdev/bin/python" jedi:server-script)))
(require 'company-jedi)


(add-to-list 'exec-path "~/miniconda3/bin")
(add-to-list 'exec-path "~/.cargo/bin")
(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" "~/miniconda3/bin:~/.cargo/bin:$PATH" '("PATH"))
(use-package conda
  :ensure t
  :init
  (setq conda-env-autoactivate-mode t)
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3")))

(add-hook 'conda-postactivate-hook 'jedi:stop-server)
(add-hook 'conda-postdeactivate-hook 'jedi:stop-server)

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(add-hook 'after-init-hook #'global-flycheck-mode)
;(setq-default flycheck-disable-checkers '(c/c++-clang javascript-jshint))

(setq-default flycheck-emacs-lisp-load-path 'inherit)
(setq flycheck-flake8-maximum-line-length 99)
(setq flycheck-python-pylint-executable "~/miniconda3/bin/pylint")
(setq flycheck-python-flake8-executable "~/miniconda3/bin/flake8")

;;
;; js2-mode
;;
(require 'yasnippet)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(require 'js2-refactor)
(require 'xref-js2)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)
(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; flycheck-rust
(use-package flycheck-rust
  :ensure t
  :hook (flycheck-mode . flycheck-rust-setup))

;; cargo
(use-package cargo
  :defer)

;; racer
(use-package racer
  :defer)

;; rust-mode
(use-package rust-mode
  :ensure t
  :init
  (setq company-tooltip-align-annotations t
        rust-format-on-save t
        rust-match-angle-brackets nil)
  :config
  (add-hook 'rust-mode-hook #'company-mode)
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  :bind
  (:map rust-mode-map
        ("C-i" . company-indent-or-complete-common)))
;; 참고: https://emacs.stackexchange.com/questions/51156/cargo-process-does-not-accept-user-input/51194#51194
(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "C-r") 'my-cargo-run))
(defun my-cargo-run ()
  "Build and run Rust code."
  (interactive)
  (cargo-process-run)
  (let (
      (orig-win (selected-window))
      (run-win (display-buffer (get-buffer "*Cargo Run*") nil 'visible))
    )
    (select-window run-win)
    (comint-mode)
    (read-only-mode 0)
    (select-window orig-win)
  )
)

;;
;; markdown
;;
;; 1) brew install multimarkdown
;; 2) TEST: $ cat README.md | markdown
;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-command
        "/usr/local/bin/multimarkdown")
  (setq markdown-split-window-direction 'right))

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
  (setq web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight nil
        ))

;;
;; react
;;
;; 1) npm install -g prettier standardx typescript
;; 2) ln -s ~/work/config/.eslintrc ~/
;;
(defun setup-tide-mode ()
  "Setup function for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :ensure t
  :config
  (defvar company-tooltip-align-annotations)
  (setq company-tooltip-align-annotations t)
  (add-hook 'js-mode-hook #'setup-tide-mode))

(setq company-tooltip-align-annotations t)
(add-hook 'js-mode-hook #'setup-tide-mode)
(setq flycheck-javascript-eslint-executable "/usr/local/bin/eslint")
(setq flycheck-javascript-standard-executable "/usr/local/bin/standardx")
(add-hook 'js-mode-hook 'prettier-js-mode)

(use-package prettier-js
  :ensure t
  :hook (js-mode . prettier-js-mode)
  :init
  (setq prettier-js-args
        '(
          "--arrow-parens" "avoid"
          "--bracket-spacing" "true"
          "--jsx-bracket-same-line" "true"
          "--jsx-single-quote" "true"
          "--no-bracket-spacing" "false"
          "--no-semi" "false"
          "--print-with" "80"
          "--quote-props" "as-needed"
          "--single-quote" "true"
          "--tab-width" "2"
          "--use-tabs" "false"
          "--trailing-comma" "es5"
          )))

(if (version< emacs-version "27.0")
    (with-eval-after-load 'rjsx-mode
      (define-key rjsx-mode-map "<" nil)
      (define-key rjsx-mode-map (kbd "C-d") nil)
      (define-key rjsx-mode-map ">" nil)))

;; golang
(use-package go-mode
  :mode "\\.go\\'"
  :custom (gofmt-command "goimports")
  :bind (:map go-mode-map
         ("C-c C-n" . go-run)
         ("C-c ."   . go-test-current-test)
         ("C-c f"   . go-test-current-file)
         ("C-c a"   . go-test-current-project))
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (use-package gotest)
  (use-package go-tag
    :config (setq go-tag-args (list "-transform" "camelcase"))))

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
