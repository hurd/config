;;
;; 한글 설정 (UTF-8)
;;
;; $Id: korean-utf8.el 3 2011-05-11 04:52:42Z hurd $
;;
(require 'cl)
(when enable-multibyte-characters
  (set-language-environment "Korean")
  (setq-default file-name-coding-system 'utf-8-unix)
  (setq input-method-verbose-flag nil
        input-method-highlight-flag nil)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)

  (when (display-graphic-p)
    (global-set-key "\C-\\" 'undefined))
  (add-hook 'quail-inactivate-hook 'delete-quail-completions)
  (defun delete-quail-completions ()
    (when (get-buffer "*Quail Completions*")
      (kill-buffer "*Quail Completions*")))

  (unless (display-graphic-p)
    (set-keyboard-coding-system 'utf-8)
    (set-terminal-coding-system 'utf-8))

  ;; Hangul Mail setting
  (setq-default sendmail-coding-system 'utf-8)

  ;; turn off C-h during input
  (eval-after-load "quail"
    '(progn
       (define-key quail-translation-keymap "\C-h" 'quail-delete-last-char)
       (define-key quail-translation-keymap (kbd "C-SPC") 'set-mark-command)
       (define-key quail-translation-keymap "\C-?" 'quail-translation-help)))
  (define-key global-map (kbd "C-x RET s") 'decode-coding-region))

;; copy & paste
(set-selection-coding-system
 (cond ((eq system-type 'windows-nt) 'euc-kr-dos)
       (t 'utf-8)))

;; use shift-space
(global-set-key [?\S- ] 'toggle-input-method)
