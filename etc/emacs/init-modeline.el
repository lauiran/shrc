
(setq evil-normal-state-tag   (propertize "[Normal]" 'face '((:background "green" :foreground "black")))
      evil-emacs-state-tag    (propertize "[Emacs]" 'face '((:background "orange" :foreground "black")))
      evil-insert-state-tag   (propertize "[Insert]" 'face '((:background "red") :foreground "white"))
      evil-motion-state-tag   (propertize "[Motion]" 'face '((:background "blue") :foreground "white"))
      evil-visual-state-tag   (propertize "[Visual]" 'face '((:background "grey80" :foreground "black")))
      evil-operator-state-tag (propertize "[Operator]" 'face '((:background "purple"))))

(defun my-align-right (text)
  (let* ((available-width (window-width (selected-window)))
         (text-width (length (format-mode-line text)))
         (padding-width (- available-width text-width)))
    (if (> padding-width 0)
        (concat (make-string padding-width ?\s) text)
      text)))

;; 简化 major-mode 的名字，替换表中没有的显示原名

(defun codefalling//simplify-major-mode-name ()
  "Return simplifyed major mode name"
  (let* ((major-name (format-mode-line "%m"))
         (replace-table
	  '(Emacs-Lisp "lisp"
                       Spacemacs\ buffer "buf"
                       Python "py"
                       Shell "sh"
                       Makrdown "mk"
                       GFM "GFM"
                       Org "ORG"
                       Text "text"
                       Fundamental "fun"))
         (replace-name (plist-get replace-table (intern major-name))))
    (if replace-name replace-name major-name)))

(setq-default
 mode-line-format
 (list
  ;; the buffer name; the file name as a tool tip
  '(:eval (propertize " %b" 'face 'font-lock-keyword-face
                      'help-echo (buffer-file-name)))

  ;; was this buffer modified since the last save?
  '(:eval (when (buffer-modified-p)
            (propertize "[+]"
                        'face 'font-lock-keyword-face
			'help-echo (buffer-file-name))))
                        ;;'help-echo "Buffer has been modified")))

  ;;'(:eval 'default-directory)

  ;; line and column
  " (" ;; '%02' to set to 2 chars at least; prevents flickering
  (propertize "%02l" 'face 'font-lock-type-face) ","
  (propertize "%02c" 'face 'font-lock-type-face)
  ") "

  ;; relative position, size of file
  "["
  (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
  "/"
  (propertize "%I" 'face 'font-lock-constant-face) ;; size
  "] "

  ;; the current major mode for the buffer.
  "["
  '(:eval (propertize (codefalling//simplify-major-mode-name)
  ;;'(:eval (propertize "%m"
		      'face 'font-lock-string-face
                      'help-echo buffer-file-coding-system))
  "] "

  "[" ;; insert vs overwrite mode, input-method in a tooltip
  '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                      'face 'font-lock-preprocessor-face
                      'help-echo (concat "Buffer is in "
                                         (if overwrite-mode "overwrite" "insert") " mode")))

  ;; is this buffer read-only?
  '(:eval (when buffer-read-only
            (concat ","  (propertize "RO"
                                     'face 'font-lock-type-face
                                     'help-echo "Buffer is read-only"))))
  "] "

  "[" ;; encode
  '(:eval (format "%s" (symbol-name buffer-file-coding-system)))
  "] "

  ;; evil state
  ;;'(:eval (evil-generate-mode-line-tag evil-state))
  ;;" "

  ;; add the time, with the date and the emacs uptime in the tooltip
  ;;'(:eval (propertize (format-time-string "%H:%M")
  ;;                    'help-echo
  ;;                    (concat (format-time-string "%c; ")
  ;;                            (emacs-uptime "Uptime:%hh"))))
  '(:eval (propertize "%z" 'face 'font-lock-type-face
		      'help-echo buffer-file-coding-system))
  ;; i don't want to see minor-modes; but if you want, uncomment this:
  ;; minor-mode-alist  ;; list of minor modes
  ;;"%-" ;; fill with '-'
  ))

(provide 'init-modeline)
