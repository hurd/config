;;
;; date-time format
;;
(setq dayname-k-alist
      '(("Sun" . "日") ("Mon" . "月") ("Tue" . "火") ("Wed" . "水")
        ("Thu" . "木") ("Fri" . "金") ("Sat" . "土")))
;(setq display-time-string-forms
;      '((format "%s年%s月%s日(%s) %s:%s %s"
;                year month day
;                (cdr (assoc dayname dayname-k-alist))
;                24-hours minutes
;                load)))
(display-time)

;;
;; insert current date string
;;
(defun insert-date ()
  "insert date in 'yyyymm-dd hh:mm:ss' format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))
(global-set-key (kbd "<f4>") 'insert-date)
