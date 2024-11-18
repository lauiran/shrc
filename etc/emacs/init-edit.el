
;; replace selected part
(delete-selection-mode t)

;; auto reload modified file
;;(global-auto-revert-mode t)

;; no backup file
(setq make-backup-files nil)
(setq auto-save-mode nil)
(setq auto-save-default nil)

;; backup to a directory
(setq-default
 backup-by-copying t ;; auto backup
 backup-directory-alist '(("." . "~/.emacs.d/.backups")) ;; backup to ~/.em_backup
 delete-old-versions t ;; auto delete old version
 kept-new-versions 3   ;; save newest 3 backup ver
 kept-old-versions 1   ;; kept 1 oldest ver
 version-control t)

;; no lockfiles
(setq create-lockfiles nil)

;; recent file 10 items
(require 'recentf)
(recentf-mode t)
(setq recentf-max-menu-item 12)

;; 重用唯一的一个缓冲区作为 Dired Mode 显示专用缓冲区
(put 'dired-find-alternate-file 'disabled nil)

;; 主动加载 Dired Mode
;; (require 'dired)
;; (defined-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; 延迟加载 Dired Mode
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;; yes or no use y/n
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init-edit)
