(defun reload-dotemacs ()
  "reload the .emacs file"
  (interactive)
  (load-file dotemacs))

(defun open-dotemacs ()
  "open the .emacs file in a new buffer"
  (interactive)
  (find-file dotemacs))

(defun open-sitelisp ()
  "open the sitelisp directory"
  (interactive)
  (find-file site-lisp))

(defun gtd ()
  "my GTD org file"
  (interactive)
  (find-file "~/GTD/gtd.org"))

(defun open-gtd ()
  (interactive)
  (gtd))

(defun open-journal ()
  "my journal file"
  (interactive)
  (find-file "~/GTD/journal.org"))

(defun journal ()
  (interactive)
  (open-journal)
  (end-of-buffer)
  (insert "\n** ")
  (insert-time0)
  (insert " "))

(defun todo (message)
  (interactive "sMessage: ")
  (insert " # TODO " (format-time-string "%d/%m/%y") " TO "  message "\n"))

(defun refactor (message)
  (interactive "sMessage: ")
  (insert " # REFACTOR " (format-time-string "%d/%m/%y") " TO "  message "\n"))

(defun comment (message)
  (interactive "sMessage: ")
  (insert " # " (format-time-string "%d/%m/%y") " TO "  message "\n"))


(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun ascii-table ()
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (beginning-of-buffer))

(defun date-time-string ()
  (format-time-string "[%Y-%m-%d %R]"))

(defun insert-time ()
  (interactive)
  (insert (date-time-string)))

(defun date-filename ()
  (format-time-string "%d-%m-%Y"))

(defun date-string ()
  (interactive)
  (format-time-string "[%Y-%m-%d]"))

(defun insert-date ()
  (interactive)
  (insert (date-string)))

; ---
; copy line without selection
; http://www.emacswiki.org/cgi-bin/wiki/CopyWithoutSelection
(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "P")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (copy-region-as-kill beg end)))

(global-set-key (kbd "C-c l") 'copy-line)

(provide 'common-functions)
