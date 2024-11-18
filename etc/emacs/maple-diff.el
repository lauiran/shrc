;;; maple-diff.el ---  show diff sign on fringe or margin.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/honmaple/emacs-maple-diff

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; show diff sign on fringe or margin
;;

;;; Code:
(require 'vc)

(defgroup maple-diff nil
  "Show diff sign on fringe or window margin."
  :group 'maple)

(defcustom maple-diff:buffer "*maple-diff*"
  "The diff buffer would be parsed."
  :type 'string
  :group 'maple-diff)

(defcustom maple-diff:regexp
  "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
  "The regexp of parsing diff info ."
  :type 'string
  :group 'maple-diff)

(defcustom maple-diff:delay 0.01
  "The delay time to update diff sign."
  :type 'float
  :group 'maple-diff)

(defcustom maple-diff:side (if (display-graphic-p) 'right-fringe 'left-margin)
  "The type of sign show."
  :type '(choice (const left-fringe)
                 (const right-fringe)
                 (const left-margin)
                 (const right-margin))
  :group 'maple-diff)

(defcustom maple-diff:change-hooks
  '(after-save-hook after-revert-hook find-file-hook)
  "The hooks of auto set diff sign."
  :type 'list
  :group 'maple-diff)

(defcustom maple-diff:ignore-alist nil
  "The buffer name or regexp that be ignored."
  :type 'list
  :group 'maple-diff)

(defcustom maple-diff:output-alist
  '((Git . maple-diff:git-output))
  "The diff function."
  :type '(cons)
  :group 'maple-diff)

(defcustom maple-diff:fringe-sign-alist
  '((added . maple-diff:added-fringe)
    (deleted . maple-diff:deleted-fringe)
    (changed . maple-diff:changed-fringe))
  "The fringe sign when side is fringe."
  :type '(cons)
  :group 'maple-diff)

(defcustom maple-diff:margin-sign-alist
  '((added . "+")
    (deleted . "-")
    (changed . "!"))
  "The margin sign when side is margin."
  :type '(cons)
  :group 'maple-diff)

(defcustom maple-diff:face-alist
  '((added . maple-diff:added)
    (deleted . maple-diff:deleted)
    (changed . maple-diff:changed))
  "The face of diff sign."
  :type '(cons)
  :group 'maple-diff)

(defface maple-diff:added
  '((t (:foreground "#A6E22E" :weight bold :inherit default)))
  "Face of added"
  :group 'maple-diff)

(defface maple-diff:deleted
  '((t (:foreground "#F92672" :weight bold :inherit default)))
  "Face of deleted"
  :group 'maple-diff)

(defface maple-diff:changed
  '((t (:foreground "#66D9EF" :weight bold :inherit default)))
  "Face of changed"
  :group 'maple-diff)

(defvar-local maple-diff:overlays nil)

(defvar-local maple-diff:margins nil)

(define-fringe-bitmap 'maple-diff:added-fringe
  [24 24 24 255 255 24 24 24])

(define-fringe-bitmap 'maple-diff:deleted-fringe
  [0 0 0 255 255 0 0 0])

(define-fringe-bitmap 'maple-diff:changed-fringe
  [24] nil nil '(center repeated))

(defun maple-diff:alist (type alist)
  "Get value of TYPE within ALIST."
  (cdr (or (assq type alist) (assq t alist))))

(defun maple-diff:ignore (&optional buffer vc)
  "Ignore BUFFER VC."
  (let ((name (buffer-file-name (or buffer (current-buffer)))))
    (or (not name)
        (cl-loop
         for r in maple-diff:ignore-alist
         when (and (stringp r) (string-match r name))
         return t)
        (and vc (not vc-mode) (not (vc-state name))))))

(defun maple-diff:set-window-margin(side)
  "Set window margin width with SIDE."
  (let ((margin (window-margins)))
    (cond ((eq side 'left-margin)
           (set-window-margins nil 1 (cdr margin))
           (setq maple-diff:margins margin))
          ((eq side 'right-margin)
           (set-window-margins nil (car margin) 1)
           (setq maple-diff:margins margin)))))

(defun maple-diff:before-string(type side)
  "Get before string with TYPE SIDE &OPTIONAL FACE."
  (let ((face (maple-diff:alist type maple-diff:face-alist))
        (margin-sign (maple-diff:alist type maple-diff:margin-sign-alist))
        (fringe-sign (maple-diff:alist type maple-diff:fringe-sign-alist)))
    (cond ((memq side '(left-fringe right-fringe))
           (propertize " " 'display `(,side ,fringe-sign . ,(when face (cons face nil)))))
          ((memq side '(left-margin right-margin))
           (propertize " " 'display `((margin ,side) ,(propertize margin-sign 'face face)))))))

(defun maple-diff:show-sign(beg end type side)
  "Show diff sign with BEG END TYPE SIDE."
  (let ((overlay (make-overlay beg end))
        (display (maple-diff:before-string type side)))
    (save-excursion
      (goto-char beg)
      (while (and (<= (point) end) (< (point) (point-max)))
        (let ((ov (make-overlay (point) (point))))
          (overlay-put ov 'maple-diff t)
          (overlay-put ov 'before-string display)
          (goto-char (point-at-bol 2)))))
    (overlay-put overlay 'maple-diff t) overlay))

(defun maple-diff:git-output(files &optional buffer)
  "Get FILES git diff output to BUFFER."
  (vc-git-command
   buffer 1 files
   "-c" "diff.autorefreshindex=0" "diff" "--no-color"
   "--no-ext-diff" "--relative" "-U0" "--"))

(defmacro maple-diff:switches (body)
  "Set default switche and excute BODY."
  `(let ((vc-git-diff-switches nil)
         (vc-hg-diff-switches nil)
         (vc-svn-diff-switches nil)
         (vc-diff-switches '("-U0")))
     ,body))

(defun maple-diff:changes()
  "Get all diff change."
  (let* ((file (buffer-file-name))
         (backend (vc-backend file))
         (diffout (maple-diff:alist backend maple-diff:output-alist))
         (buffer maple-diff:buffer) results)
    (when backend
      (if diffout (funcall diffout (list file) buffer)
        (maple-diff:switches
         (vc-call-backend backend 'diff (list file) nil nil buffer)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (while (re-search-forward maple-diff:regexp nil t)
          (let ((headno (string-to-number (match-string 1)))
                (headcnt (string-to-number (or (match-string 2) "1")))
                (curno (string-to-number (match-string 3)))
                (curcnt (string-to-number (or (match-string 4) "1"))))
            (ignore headno)
            (push
             (cond ((< headcnt curcnt)
                    (cons (cons curno (+ curno (- curcnt 1)))
                          (if (= headcnt 0) 'added 'changed)))
                   ((> headcnt curcnt)
                    (cons (cons curno (+ curno curcnt)) 'deleted))
                   ((= headcnt curcnt)
                    (cons (cons curno (+ curno (- curcnt 1))) 'changed)))
             results))))
      (reverse results))))

(defun maple-diff:next-sign(arg)
  "Goto next sign if ARG."
  (interactive "p")
  (unless maple-diff:overlays
    (error "There are no changes!"))
  (let* ((pre    (< arg 0))
         (point    (point))
         (overlays (if pre (reverse maple-diff:overlays) maple-diff:overlays)))
    (goto-char
     (or (cl-loop for overlay in overlays
                  when (if pre (< (overlay-end overlay) point)
                         (> (overlay-start overlay) point))
                  return (overlay-start overlay))
         (overlay-start (car overlays))))))

(defun maple-diff:previous-sign(arg)
  "Goto previous sign if not ARG."
  (interactive "p")
  (maple-diff:next-sign (- arg)))

(defun maple-diff:hide()
  "Hide diff sign on fringe or margin."
  (mapc (lambda(overlay)
          (let ((beg (overlay-start overlay))
                (end (overlay-end overlay)))
            (when (and beg end)
              (cl-loop for ov in (overlays-in beg (1+ end))
                       when (overlay-get ov 'maple-diff)
                       do (delete-overlay ov)))
            (delete-overlay overlay)))
        maple-diff:overlays)
  (setq maple-diff:overlays nil))

(defun maple-diff:show()
  "Show diff sign on fringe or margin."
  (unless (maple-diff:ignore nil t)
    (setq maple-diff:overlays
          (cl-loop
           for ((beg . end) . type) in (maple-diff:changes)
           collect
           (let ((bep (save-excursion
                        (goto-char (point-min))
                        (forward-line (1- beg))
                        (point)))
                 (enp (save-excursion
                        (goto-char (point-min))
                        (forward-line (1- end))
                        (point))))
             (maple-diff:show-sign bep enp type maple-diff:side))))
    (maple-diff:set-window-margin maple-diff:side)))

;;;###autoload
(define-minor-mode maple-diff-mode
  "Show diff with HEAD."
  :group  'maple-diff
  (if maple-diff-mode (maple-diff-mode-on) (maple-diff-mode-off)))

;;;###autoload
(define-global-minor-mode global-maple-diff-mode maple-diff-mode
  (lambda() (unless (maple-diff:ignore) (maple-diff-mode 1))))

(defun maple-diff:update()
  "Update diff sign."
  (interactive)
  (when maple-diff-mode
    (run-with-idle-timer
     maple-diff:delay nil
     (lambda() (maple-diff:hide) (maple-diff:show)))))

(defun maple-diff-mode-on ()
  "Toggle on `maple-diff-mode`."
  (interactive)
  (maple-diff:show)
  (dolist (hook maple-diff:change-hooks)
    (add-hook hook 'maple-diff:update nil t)))

(defun maple-diff-mode-off ()
  "Toggle off `maple-diff-mode`."
  (interactive)
  (maple-diff:hide)
  (dolist (hook maple-diff:change-hooks)
    (remove-hook hook 'maple-diff:update t))
  (when maple-diff:margins
    (set-window-margins (get-buffer-window) (car maple-diff:margins) (cdr maple-diff:margins))
    (setq maple-diff:margins nil)))

(provide 'maple-diff)
;;; maple-diff.el ends here
