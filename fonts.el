;; -*- Mode: Emacs-Lisp -*-
;; $Id: fonts.el 3 2011-05-11 04:52:42Z hurd $
;;

;; default font
;; (set-face-font 'default "Monaco-10")

(when (eq system-type 'darwin)

  (setq use-default-font-for-symbols nil)
  (setq inhibit-compacting-font-caches t)
  (setq default-font-family "Hack Nerd Font")

  ;; 한글 자모
  (set-fontset-font "fontset-default" '(#x1100 . #x11ff) '("D2Coding" . "iso10646"))
  ;; Currency Symbols
  (set-fontset-font "fontset-default" '#x20a9 '("D2Coding" . "iso10646"))
  ;; CJK Symbols and Punctuation
  (set-fontset-font "fontset-default" '(#x302e . #x302f) '("D2Coding" . "iso10646"))
  ;; Hangul Compatibility Jamo
  (set-fontset-font "fontset-default" '(#x3130 . #x318f) '("D2Coding" . "iso10646"))
  ;; Enclosed CJK Letters and Months
  (set-fontset-font "fontset-default" '(#x3200 . #x321e) '("D2Coding" . "iso10646"))
  ;; Enclosed CJK Letters and Months 와 KS 기호의 글꼴
  (set-fontset-font "fontset-default" '(#x3260 . #x327f) '("D2Coding" . "iso10646"))
  ;; Hangul Jamo Extended-A
  (set-fontset-font "fontset-default" '(#xa960 . #xa97f) '("D2Coding" . "iso10646"))
  ;; Hangul Syllables
  (set-fontset-font "fontset-default" '(#xac00 . #xd7a3) '("D2Coding" . "iso10646"))
  ;; Hangul Jamo Extended-B
  (set-fontset-font "fontset-default" '(#xd7b0 . #xd7ff) '("D2Coding" . "iso10646"))
  ;; Halfwidth and Fulwidth Forms
  (set-fontset-font "fontset-default" '(#xffa1 . #xffdc) '("D2Coding" . "iso10646"))
  ;; Fullwidth Forms
  (set-fontset-font "fontset-default" '#xffe6 '("D2Coding" . "iso10646")))
