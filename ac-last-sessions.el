;;; ac-last-sessions.el -- Auto-complete source to complete words across sessions

;; Copyright (C) 2017- zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 0.0.0
;; Package-Requires: ()

;;; Commentary:

;; * description
;;
;; This package provides a auto-complete source
;; `ac-source-last-sessions', which is like
;; `ac-source-words-in-same-mode-buffers' but saved across sessions.
;;
;; * Installation
;;
;; Put this scrpit into a 'load-path'ed directory, and load it in your
;; init file.
;;
;;   (require 'ac-last-sessions)
;;
;; Set `ac-last-sessions-save-file'.
;;
;;   (setq ac-last-sessions-save-file "~/.emacs.d/.ac-last-sessions")
;;
;; And add hooks to save/load the words.
;;
;;   (add-hook 'emacs-startup-hook 'ac-last-sessions-load-completions)
;;   (add-hook 'kill-emacs-hook 'ac-last-sessions-save-completions)
;;
;; Then `ac-source-last-sessions' is available.
;;
;;   (push 'ac-source-last-sessions ac-sources)

;;; Change Log:

;; 0.0.0 text release

;;; Code:

(require 'auto-complete)

(defgroup ac-last-sessions nil
  "Auto-complete source to complete words across sessions."
  :group 'auto-complete)

(defcustom ac-last-sessions-save-file nil
  "Filename to save words in."
  :type 'string
  :group 'ac-last-sessions)

(defcustom ac-last-sessions-saved-words 10000
  "Number of words (per language) to save."
  :type 'integer
  :group 'ac-last-sessions)

(defcustom ac-last-sessions-minimun-word-length 5
  "Minimum length of word to save."
  :type 'integer
  :group 'ac-last-sessions)

(defcustom ac-last-sessions-major-modes '(prog-mode)
  "List of major-modes to enable saving sessions."
  :type '(list symbol)
  :group 'ac-last-sessions)

(defvar ac-last-sessions--candidates nil
  "Store words in the form (MODE . (WORD ...)).")

;; * utils

(defun ac-last-sessions--filter (pred lst)
  "Destructive filter function."
  (while (and lst (not (funcall pred (car lst)))) ; drop first elems
    (setq lst (cdr lst)))
  (let ((tmp lst))
    (while (cdr tmp)
      (if (funcall pred (cadr tmp))
          (setq tmp (cdr tmp))
        (setcdr tmp (cddr tmp)))))
  lst)

(defun ac-last-sessions--some (pred lst)
  "Return non-nil if some of elements in LST satisfies PRED. PRED
is considered as a pure function."
  (let ((res nil))
    (while (and (not res) lst)
      (when (funcall pred (pop lst))
        (setq res t)))
    res))

;; * core

(defun ac-last-sessions-load-completions ()
  "Load saved words from `ac-last-sessions-save-file'."
  (when (and ac-last-sessions-save-file
             (file-exists-p ac-last-sessions-save-file))
    (with-temp-buffer
      (insert-file-contents ac-last-sessions-save-file)
      (goto-char (point-min))
      (setq ac-last-sessions--candidates (read (current-buffer))))
    (message "ac-last-sessions: last session loaded.")))

(defun ac-last-sessions-save-completions ()
  "Save words in `ac-last-sessions-save-file'."
  (let ((word-regexp
         (format "\\(?:\\s_\\|\\sw\\)\\{%d,\\}" ac-last-sessions-minimun-word-length)))
    (when ac-last-sessions-save-file
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (ac-last-sessions--some 'derived-mode-p ac-last-sessions-major-modes)
            (let ((cell (assq major-mode ac-last-sessions--candidates))
                  (words (ac-last-sessions--filter
                          (lambda (x) (string-match word-regexp x))
                          (split-string
                           (buffer-substring-no-properties (point-min) (point-max))
                           "\\(?:^\\|\\_>\\).*?\\(?:\\_<\\|$\\)"))))
              (if cell
                  (nconc (cdr cell) words)
                (push (cons major-mode words) ac-last-sessions--candidates))))))
      (dolist (pair ac-last-sessions--candidates)
        (setcdr pair (delete-dups (cdr pair)))
        (let ((pair (nthcdr (1- ac-last-sessions-saved-words) ac-last-sessions--candidates)))
          (when pair (setcdr pair nil))))
      (with-temp-buffer
        (prin1 ac-last-sessions--candidates (current-buffer))
        (write-region (point-min) (point-max) ac-last-sessions-save-file))
      (message "ac-last-sessions: session saved."))))

(ac-define-source last-sessions
  '((candidates . (assoc-default major-mode ac-last-sessions--candidates))))

(provide 'ac-last-sessions)

;;; ac-last-sessions.el ends here
