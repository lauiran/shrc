;; org-mode long line break
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;; load languages
;;(org-babel-do-load-languages
;; 'org-babel-load-languages
;; '(
;;   (sh . t)
;;   (python . t)
;;   (R . t)
;;   (ruby . t)
;;   (ditaa . t)
;;   (dot . t)
;;   (octave . t)
;;   (sqlite . t)
;;   (perl . t)
;;   (C . t)
;;   ))

(provide 'init-orgmode)
