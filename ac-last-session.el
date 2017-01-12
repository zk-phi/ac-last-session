;; ac-last-session

(require 'auto-complete)

(defgroup ac-last-session nil
  "auto-complete source to complete words-in-same-mode-buffers in
the last session."
  :group 'auto-complete)

(defcustom ac-last-session-save-file nil
  "Filename to save ac-words-in-same-mode-buffers candidates."
  :type 'string
  :group 'ac-last-session)

(defcustom ac-last-session-saved-words 100000
  "Number of words (per language) to keep in the history."
  :type 'integer
  :group 'ac-last-session)

(defvar ac-last-session--candidates nil)

(defun ac-last-session-load-saved-session ()
  (when (and ac-last-session-save-file
             (file-exists-p ac-last-session-save-file))
    (with-temp-buffer
      (insert-file-contents ac-last-session-save-file)
      (goto-char (point-min))
      (setq ac-last-session--candidates (read (current-buffer))))
    (message "ac-last-session: last session loaded.")))

(defun ac-last-session-save-session ()
  (when ac-last-session-save-file
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (let ((cell (assq major-mode ac-last-session--candidates))
              (words (split-string
                      (buffer-substring-no-properties (point-min) (point-max))
                      "\\(?:^\\|\\_>\\).*?\\(?:\\_<\\|$\\)")))
          (if cell
              (nconc (cdr cell) words)
            (push (cons major-mode words) ac-last-session--candidates)))))
    (dolist (pair ac-last-session--candidates)
      (setcdr ac-last-session--candidates (delete-dups (cdr ac-last-session--candidates)))
      (let ((pair (nthcdr (1- ac-last-session-saved-words) ac-last-session--candidates)))
        (when pair (setcdr pair nil))))
    (with-temp-buffer
      (prin1 ac-last-session--candidates (current-buffer))
      (write-region (point-min) (point-max) ac-last-session-save-file))
    (message "ac-last-session: session saved.")))

(ac-define-source last-session
  '((candidates . (assoc-default major-mode ac-last-session--candidates))))

(provide 'ac-last-session)
