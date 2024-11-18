
(setq my-auth-name "lawhibao")
(setq my-mail-addr "ailunny@gmail.com")

;; enable auto insert minor mode
(auto-insert-mode)

;; disable auto-insert query
(setq-default auto-insert-query nil)

;; auto insert file headers
(setq-default
 auto-insert-alist
 '(
   ((python-mode . "Python program") nil (my-header-py) (my-goto-line 4))
   ((sh-mode . "Shell script") nil "#!/bin/bash\n\n")
   ((c-mode . "C program") nil
    (if (my-header-is-hfile) (my-header-h) (my-header-c))
    (if (my-header-is-hfile) (my-goto-line 4) (my-goto-line 11)))
   ((c++-mode . "Cpp program") nil
    (if (my-header-is-hfile) (my-header-hpp) (my-header-cpp))
    (if (my-header-is-hfile) (my-goto-line 8) (my-goto-line 11)))
   ))

(defun my-goto-line (n)
  (goto-line n)
  "")

(defun my-header-py ()
  (concat
   "#!/usr/bin/env python\n"
   "# -*- coding: utf-8 -*-\n"
   "\n\n\n"
   "'''\nif __name__ == '__main__':\n"
   "    test code\n'''\n\n"
   ))

(defun my-header-is-hfile ()
  (equal "h"
	 (downcase (substring (car (last (split-string (buffer-name) "\\."))) 0 1))
	 ))

(defun my-header-c ()
  (concat
   "/*\n"
   "    File : " (buffer-name) "\n"
   "    Auth : " my-auth-name "\n"
   "    Mail : " my-mail-addr "\n"
   "    Time : " (format-time-string "%Y/%m/%d %H:%M:%S") "\n"
   "*/\n"
   "\n#include <stdio.h>\n"
   "\n\n\n\n"
   ))

(defun my-h-fname ()
  (concat
   "_"
   (upcase (file-name-base (buffer-name)))
   "_"
   (upcase (file-name-extension (buffer-name)))
   ))

(defun my-header-h ()
  (concat
   "#ifndef " (my-h-fname) "\n"
   "#define " (my-h-fname) " 1\n"
   "\n\n\n\n"
   "#endif /* " (my-h-fname) " */\n\n"
   ))

(defun my-header-cpp ()
  (concat
   "/*\n"
   "    File : " (buffer-name) "\n"
   "    Auth : " my-auth-name "\n"
   "    Mail : " my-mail-addr "\n"
   "    Time : " (format-time-string "%Y/%m/%d %H:%M:%S") "\n"
   "*/\n"
   "\n#include <iostream>\n"
   "\n\n\n\n"
   ))

(defun my-header-hpp ()
  (concat
   "#ifndef " (my-h-fname) "\n"
   "#define " (my-h-fname) " 1\n"
   "\n#ifdef __cplusplus\nextern \"C\" {\n#endif\n"
   "\n\n\n\n"
   "#ifdef __cplusplus\n} /* extern \"C\" */\n#endif\n\n"
   "#endif /* " (my-h-fname) " */\n\n"
   ))

(provide 'init-autoinsert)
