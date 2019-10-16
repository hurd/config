;; -*- Mode: Emacs-Lisp -*-
;;
;; $Id$
;;
;; look & feel
;;

;; frame
(add-to-list 'default-frame-alist '(width . 120))
(setq frame-title-format
      (setq icon-title-format
            (list "%b (%p) by "
                  (getenv "USERNAME"))))

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
  ;; turn off toolbar
  (tool-bar-mode 0))

;; 2.6
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

;;
;; Theme
;;
(use-package monokai-theme
 :ensure t
 :load-path "themes"
 :init
 (load-theme 'monokai t))
