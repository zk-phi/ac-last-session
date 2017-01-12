;; ac-last-sessions

(require 'auto-complete)

(defgroup ac-last-sessions nil
  "auto-complete source to complete words-in-same-mode-buffers in
the last session."
  :group 'auto-complete)

(defcustom ac-last-sessions-save-file nil
  "Filename to save ac-words-in-same-mode-buffers candidates."
  :type 'string
  :group 'ac-last-sessions)

(defcustom ac-last-sessions-saved-words 10000
  "Number of words (per language) to keep in the history."
  :type 'integer
  :group 'ac-last-sessions)

(defvar ac-last-sessions--candidates nil)

(defun ac-last-sessions--filter (pred lst)
  "Destructive filter function."
  (while (and lst (not (funcall pred (car lst)))) ; drop first elems
    (setq lst (cdr lst)))
  (let ((tmp lst))
    (while (cdr tmp)
      (if (funcall pred(cadr tmp))
          (setq tmp (cdr tmp))
        (setcdr tmp (cddr tmp)))))
  lst)

(defun ac-last-sessions-load-saved-session ()
  (when (and ac-last-sessions-save-file
             (file-exists-p ac-last-sessions-save-file))
    (with-temp-buffer
      (insert-file-contents ac-last-sessions-save-file)
      (goto-char (point-min))
      (setq ac-last-sessions--candidates (read (current-buffer))))
    (message "ac-last-sessions: last session loaded.")))

(defun ac-last-sessions-save-session ()
  (when ac-last-sessions-save-file
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (let ((cell (assq major-mode ac-last-sessions--candidates))
              (words (ac-last-sessions--filter
                      (lambda (x) (string-match "\\(?:\\s_\\|\\sw\\)\\{5,\\}" x))
                      (split-string
                       (buffer-substring-no-properties (point-min) (point-max))
                       "\\(?:^\\|\\_>\\).*?\\(?:\\_<\\|$\\)"))))
          (if cell
              (nconc (cdr cell) words)
            (push (cons major-mode words) ac-last-sessions--candidates)))))
    (dolist (pair ac-last-sessions--candidates)
      (setcdr pair (delete-dups (cdr pair)))
      (let ((pair (nthcdr (1- ac-last-sessions-saved-words) ac-last-sessions--candidates)))
        (when pair (setcdr pair nil))))
    (with-temp-buffer
      (prin1 ac-last-sessions--candidates (current-buffer))
      (write-region (point-min) (point-max) ac-last-sessions-save-file))
    (message "ac-last-sessions: session saved.")))

(ac-define-source last-sessions
  '((candidates . (assoc-default major-mode ac-last-sessions--candidates))))

(provide 'ac-last-sessions)
