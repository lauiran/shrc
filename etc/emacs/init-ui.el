; encode
;(prefer-coding-system 'gbk)
;(setq-default buffer-file-coding-system 'gbk)

; pixels x y from upper left
;(set-frame-position (selected-frame) 120 24)
; rows and columns w h
;(set-frame-size (selected-frame) 117 42)

(add-to-list 'default-frame-alist '(left . 120))
(add-to-list 'default-frame-alist '(top . 24))
(add-to-list 'default-frame-alist '(height . 42))
(add-to-list 'default-frame-alist '(width . 117))
;;(add-to-list 'default-frame-alist '(font . "Code New Roman-10.5"))
(add-to-list 'default-frame-alist '(font . "Consolas-10.5"))

;; set frame title as filename
;;(setq frame-title-format "%b")
(setq frame-title-format
      '(:eval (concat
	       (buffer-name)
	       (if (and buffer-file-name (buffer-modified-p)) " +")
	       (if buffer-file-name
		   (concat " (" (directory-file-name (abbreviate-file-name default-directory)) ")"))
	       " - Emacs")))

;; 关闭工具栏，tool-bar-mode即为一个Minor Mode
(tool-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

;; show line number
(global-linum-mode t)
(setq linum-format (lambda (line) (propertize (format (let ((w (length (number-to-string (count-lines (point-min) (point-max)))))) (concat "%" (number-to-string w) "d ")) line) 'face 'linum)))

;; high-light current line
(global-hl-line-mode t)

;; 关闭启动帮助画面
(setq inhibit-splash-screen t)

;; set cursor type
(setq-default cursor-type 'bar)
(blink-cursor-mode t)
(setq-default blink-cursor-blinks 0)

;; set default font
;;(set-frame-font "Code New Roman" t t)
(set-frame-font "Consolas" t t)
;; set font for emoji
(set-fontset-font t '(#x1f300 . #x1fad0) "CaskaydiaCove NF-12")
;; chs
(set-fontset-font t '(#x4e00 . #x9fff) "Microsoft Yahei-12")
;; cht
(set-fontset-font t '(#x3400 . #x4dff) "Microsoft Yahei-12")
;; chinese ext
(set-fontset-font t '(#x20000 . #x32000) "Microsoft Yahei-12")
;; 偏旁部首
(set-fontset-font t '(#x2e80 . #x31ff) "Microsoft Yahei-12")
;; 部件扩展
(set-fontset-font t '(#xe400 . #xfb00) "Microsoft Yahei-12")
;; 字体测试
;; abcdefghijklmnopqrstuvwxyz 0123456789 ~!@#$%^&*()_+
;; 中文字体测试呢 重复
;; aaaaaaaaaaaaaaaaaaaa
;; iiiiiiiiiiiiiiiiiiii
;; jjjjjjjjjjjjjjjjjjjj

;; fullscreen
;;(setq  initial-frame-alist (quote ((fullscreen . maximized))))

;; no bell warning
(setq ring-bell-function 'ignore)

;; long line
;;(fringe-mode '(0 . nil))
(fringe-mode '(0 . 0))

;; margin
(setq-default left-margin-width 1 right-margin-width 1)
(set-window-buffer nil (current-buffer))

;; apply theme
(load-file "~/.dotusr/etc/emacs/e6e1-theme.el")
(load-theme 'e6e1 t)

(provide 'init-ui)
