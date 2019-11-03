;; -*- Mode: Emacs-Lisp -*-
;;
;; $Id$
;;
;; look & feel
;;

;; frame
(add-to-list 'default-frame-alist '(width . 120))
(setq frame-title-format
      '(buffer-file-name "%f"
                         (dired-directory dired-directory "%b")))

;; enablling font-lock globally
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; line length is 80 chars
(setq fill-column 120)
(setq-default comment-coulumn 120)

;; parentheses highlighting
(show-paren-mode 1)
(setq blink-matching-paren t)

;; transient mark mode
(transient-mark-mode 1)
(setq mark-even-if-inactive t)

;; highlight during query
(setq query-replace-highlight t)

;; highlight incremental search
(setq search-highlight t)

;; visual bell
(setq visible-bell t)
;; scroll with less jump
(setq scroll-step 1)
;; set this to make scolloing faster
(setq lazy-lock-defer-on-scrolling t)

;; disable startup message
(setq inhibit-startup-message t)

;; GUI environment
(when (display-graphic-p)
  ;; fontset
  (load "fonts")
  ;; mouse wheel
  (mwheel-install)
  ;; frame height
  (add-to-list 'default-frame-alist '(height . 60))
  ;; turn off scroll bar
  (scroll-bar-mode -1)
  ;; turn off menu bar
  (menu-bar-mode t)
  ;; display line and column
  (setq-default line-number-mode t)
  (setq-default column-number-mode t)
  ;; display time
  (setq display-time-24hr-format t)
  (display-time)
  ;; turn on image viewing
  (auto-image-file-mode t)
  ;; turn off/on cursor blinking
  (blink-cursor-mode t)
  ;; desktop
  (use-package desktop
    :ensure t
    :init
    (desktop-save-mode 1))
  ;; windmove
  (use-package windmove
    :config
    ;; use shift + arrow keys
    (windmove-default-keybindings))
  ;; VsCode Icons for emacs
  (use-package vscode-icon
    :ensure t
    :commands (vscode-icon-for-file))
  ;; all-the-icons
  (use-package all-the-icons)
  ;; posframe
  (use-package posframe)
  ;; hyrdra
  ;(use-package hydra
  ;  :config
  ;  (use-package hydra-posframe
  ;    :load-path "~/.emacs.d/hydra-posframe"
  ;    :custom
  ;    (hydra-posframe-parameters
  ;     '((left-fringe . 5)
  ;       (right-fringe . 5)))
  ;    :custom-face
  ;    (hydra-posframe-border-face ((t (:background "#6272a4"))))
  ;    :hook (after-init . hydra-posframe-enable)))
  ;; turn off toolbar
  (tool-bar-mode 0))

;; dimmer
(use-package dimmer
  :disabled
  :custom
  (dimmer-fraction 0.5)
  (dimmer-exclusion-regexp-list
       '(".*Minibuf.*"
         ".*which-key.*"
         ".*NeoTree.*"
         ".*Messages.*"
         ".*Async.*"
         ".*Warnings.*"
         ".*LV.*"
         ".*Ilist.*"))
  :config
  (dimmer-mode t))

;; 2.6 line numbers mode
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; enable mouse reporting for terminal emulators
(unless (display-graphic-p)
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down-line 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )

;; 3ibuffer-sidebar
(use-package ibuffer-sidebar
  :ensure t
  :commands (ibuffer-sidebar-toggle-sidebar))

(defun sidebar-toggle()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

;;  dired-sidebar
(use-package dired-sidebar
  :bind (("C-x C-n" . sidebar-toggle))
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

;; which-key
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;; guide-key
(use-package guide-key
  :diminish guide-key-mode
  :config
  (guide-key-mode t)
  (setq guide-key/highlight-command-regexp
        '('"rectangle"
          ("register" . font-lock-type-face)
          ("bookmark" . "hot pink")
          ))
  (setq guide-key/guide-key-sequence '("C-x v"   ;; version control
                                       "C-c a"   ;; my mode-specific bindings
                                       "C-c l"   ;; line-jumping
                                       "C-c r"   ;;
                                       )))

;; highlight line
(use-package hl-line
  :ensure nil
  :hook
  (after-init . global-hl-line-mode))

;; paren
(use-package paren
  :ensure nil
  :hook
  (after-init . show-paren-mode)
  :custom-face
  (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))) ;; :box t
  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; rainbow delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; rainbow mode
(use-package rainbow-mode
  :diminish
  :hook (emacs-lisp-mode . rainbow-mode))

;;
;; Theme
;;
(use-package monokai-theme
 :ensure t
 :config
 (load-theme 'monokai t))
