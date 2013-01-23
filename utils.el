;; -*- Mode: Emacs-Lisp -*-
;; $Id$
;;

;;
;; dos --> unix
;;
(defun dos2unix ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\r" nil t)
      (replace-match ""))))

