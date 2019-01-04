;;; blame-this.el --- blame this! -*- lexical-binding: t; -*-

;; Time-stamp: <2019-01-04 10:39:35>
;; Copyright (C) 2019 Pierre Lecocq

;; Author: Pierre Lecocq <pierre.lecocq@gmail.com>
;; URL: https://github.com/pierre-lecocq/blame-this
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Display a git blame summary of the current line or file.

;;; Code:

(defgroup blame-this nil
  "Display a git blame summary of the current line or file."
  :prefix "blame-this-"
  :group 'lisp)

(defcustom blame-this-line-display-mode "overlay"
  "Display mode for blaming line. Can be: overlay, minibuffer or compile."
  :type '(string)
  :group 'blame-this)

(defcustom blame-this-file-display-mode "compile"
  "Display mode for blaming file. Can be: overlay, minibuffer or compile."
  :type '(string)
  :group 'blame-this)

(defcustom blame-this-overlay-timeout 10
  "Overlay timeout."
  :type '(integer)
  :group 'blame-this)

(defface blame-this-overlay-face
  '((((class color) (background light))
     :background "grey90" :foreground "black")
    (((class color) (background dark))
     :background "grey10" :foreground "white"))
  "Overlay face."
  :group 'blame-this)

(defun blame-this--command-for-line ()
  "Build command to run for line."
  (let ((line-num (1+ (count-lines 1 (point)))))
    (format "git blame -L%d,%d --incremental %s"
            line-num line-num buffer-file-name)))

(defun blame-this--command-for-file ()
  "Build command to run for file."
  (format "git blame --incremental %s" buffer-file-name))

(defun blame-this--execute-in-minibuffer (command)
  "Execute COMMAND and display its output in minibuffer."
  (message (shell-command-to-string command)))

(defun blame-this--execute-in-compile (command)
  "Execute COMMAND and display its output in compile buffer."
  (compile command))

(defun blame-this--execute-in-overlay (command)
  "Execute COMMAND and display its output in overlay."
  (forward-line)
  (let ((ov (make-overlay (line-end-position) (line-end-position)))
        (str (shell-command-to-string command)))
    (overlay-put ov 'after-string
                 (propertize str 'face 'blame-this-overlay-face))
    (sit-for blame-this-overlay-timeout)
    (delete-overlay ov))
  (forward-line -1))

(defun blame--this (target)
  "Blame this TARGET."
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (when default-directory
      (let* ((display-val (symbol-value (intern (format "blame-this-%s-display-mode" target))))
             (execute-fn (intern (format "blame-this--execute-in-%s" display-val)))
             (command-fn (intern (format "blame-this--command-for-%s" target))))
        (funcall execute-fn (funcall command-fn))))))

(defun blame-this-line ()
  "Display blame info for the current line."
  (interactive)
  (blame--this "line"))

(defun blame-this-file ()
  "Display blame info for the current file."
  (interactive)
  (blame--this "file"))

;;;###autoload
(define-minor-mode blame-this-mode
  "Display a git blame summary of the current line or file."
  :global t
  (when blame-this-mode
    (global-set-key (kbd "C-c b l") #'blame-this-line)
    (global-set-key (kbd "C-c b f") #'blame-this-file)))

(provide 'blame-this)

;;; blame-this.el ends here
