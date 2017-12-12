;; -*- Mode: Emacs-Lisp -*-
;;
;; $Id$
;;
;; look & feel
;;

;; frame
(add-to-list 'default-frame-alist '(width . 80))
(setq frame-title-format
      (setq icon-title-format
            (list "%b (%p) by "
                  (getenv "USERNAME"))))

;; enablling font-lock globally
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; line length is 80 chars
(setq fill-column 80)
(setq-default comment-coulumn 80)

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
(when window-system
  ;; fontset
  (load "fonts")
  ;; mouse wheel
  (mwheel-install)
  ;; frame height
  (add-to-list 'default-frame-alist '(height . 40))
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

;;
;; Theme 
;;
(when (>= emacs-major-version 24)
  (load-theme 'manoj-dark t))
