(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(
               ("gnu" . "http://elpa.gnu.org/packages/")
			   ;;("melpa" . "http://melpa.org/packages/")
			   ;;("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
               ))
  )

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar my/packages '(
		      ;; --- Auto-completion ---
		      company
		      ;; --- Better Editor ---
		      hungry-delete
		      swiper
		      counsel
		      smartparens
              smart-tabs-mode
		      ;; --- Major Mode ---
		      js2-mode
		      jedi
		      helm-ag
              markdown-mode
		      ;; --- Minor Mode ---
		      nodejs-repl
		      exec-path-from-shell
		      ;;git-gutter-fringe
		      diff-hl
		      ;; --- Themes ---
		      ;;monokai-theme
		      ;;solarized-theme
		      ) "Default packages")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  (loop for pkg in my/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Find Executable Path on OS X
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t) 

;; company
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; hungry-delete
(global-hungry-delete-mode)

;; helm-ag
(global-set-key (kbd "C-c p s") 'helm-do-ag-project-root)

;; git gutter
;; If you enable global minor mode
;;(global-git-gutter-mode t)

;; If you would like to use git-gutter.el and linum-mode
;;(git-gutter:linum-setup)

;; diff-hl
(global-diff-hl-mode)
(diff-hl-flydiff-mode)

;; smart-tabs
;;(smart-tabs-insinuate 'c 'c++)
;;(add-hook 'c-mode-common-hook (lambda () (setq indent-tabs-mode t)))
(setq-default indent-tabs-mode t)
(setq-default c-basic-offset 4)

(provide 'init-packages)
