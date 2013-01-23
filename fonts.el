;; -*- Mode: Emacs-Lisp -*-
;; $Id: fonts.el 3 2011-05-11 04:52:42Z hurd $
;;

;; default font
;; (set-face-font 'default "Monaco-10")

;;
;; linux
;;
(when (eq system-type 'gnu/linux)
  ;; Korean font
  (set-fontset-font "fontset-default" '(#x1100 . #xffdc)
		    '("NanumGothicCoding" . "unicode-bmp"))
  (set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
		    '("NanumGothicCoding" . "unicode-bmp")))


(setq face-font-rescale-alist
 	'((".*nanum.*" . 1.3)))

;;
;; win32
;;
(when (or (eq window-system 'w32)
	  (eq system-type 'windows-nt))
  (set-face-font 
   'default
   "-outline-Consolas-normal-normal-normal-mono-13-*-*-*-c-*-iso10646-1")
  (set-fontset-font
   "fontset-default"
   'hangul '("NanumGothicCoding" . "unicode-bmp"))
  (setq default-directory "D:/emacs/" )
  )

