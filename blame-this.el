;;; blame-this.el --- blame this! -*- lexical-binding: t; -*-

;; Time-stamp: <2019-01-17 11:38:12>
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

;; Display a git blame summary of the current line.

;;; Code:

(defgroup blame-this nil
  "Display a git blame summary of the current line."
  :prefix "blame-this-"
  :group 'lisp)

(defcustom blame-this-on-idle nil
  "Blame the current line automatically on idle."
  :type '(boolean)
  :group 'blame-this)

(defcustom blame-this-idle-time 2
  "Idle time before triggering blame-this-line."
  :type '(integer)
  :group 'blame-this)

(defcustom blame-this-display-mode "overlay"
  "Display mode. Can be: overlay or minibuffer."
  :type '(string)
  :group 'blame-this)

(defcustom blame-this-overlay-timeout 10
  "Overlay timeout."
  :type '(integer)
  :group 'blame-this)

(defface blame-this-overlay-face
  '((t :foreground "black"
       :background "LemonChiffon"))
  "Overlay face."
  :group 'blame-this)

(defvar blame-this--timer nil)

(defun blame-this--format-output (output)
  "Format OUTPUT for the condensed setting."
  (with-temp-buffer
    (insert output)
    (save-excursion
      (goto-char (point-min))
      (let ((chunks '()))
        (while (not (eobp))
          (let ((line-num (1+ (count-lines 1 (point))))
                (line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            (cond ((= line-num 1)
                   (push (nth 0 (split-string line)) chunks))
                  ((or (= line-num 2)
                       (= line-num 3))
                   (push (mapconcat 'identity (cdr (split-string line)) " ") chunks))
                  ((= line-num 4)
                   (push (current-time-string (string-to-number (mapconcat 'identity (cdr (split-string line)) " "))) chunks)))
            (forward-line 1)))
        (mapconcat 'identity (reverse chunks) " - ")))))

(defun blame-this--display-in-overlay (output)
  "Display OUTPUT in an overlay."
  (let ((ov (make-overlay (line-end-position) (line-end-position))))
    (overlay-put ov 'after-string
                 (propertize (concat " " output) 'face 'blame-this-overlay-face))
    (sit-for blame-this-overlay-timeout)
    (delete-overlay ov)))

(defun blame-this--display-in-minibuffer (output)
  "Display OUTPUT in the minibuffer."
  (message output))

(defun blame-this--execute-command ()
  "Execute command ."
  (shell-command-to-string
   (format "git blame -L%d,+1 --incremental %s"
           (1+ (count-lines 1 (point))) buffer-file-name)))

(defun blame-this-line ()
  "Display blame info for the current line."
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (when default-directory
      (let ((output (blame-this--format-output (blame-this--execute-command))))
        (cond ((string= blame-this-display-mode "overlay")
               (blame-this--display-in-overlay output))
              ((string= blame-this-display-mode "minibuffer")
               (blame-this--display-in-minibuffer output))
              (t (error "Unsupported display mode %s" blame-this-display-mode)))))))

;;;###autoload
(define-minor-mode blame-this-mode
  "Display a git blame summary of the current line."
  :global t
  (when blame-this-mode
    (global-set-key (kbd "C-c b l") #'blame-this-line)
    (when blame-this-on-idle
      (unless blame-this--timer
        (setq blame-this--timer
              (run-with-idle-timer
               blame-this-idle-time t #'blame-this-line))))))

(provide 'blame-this)

;;; blame-this.el ends here
