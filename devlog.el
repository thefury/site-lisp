;;; devlog.el --- Development log

;; Copyright (C) 2008 Trevor Oke
;; Author: Trevor Oke <trevor@trevoroke.com>
;; Keywords: development log
;; Version: 0.1

;; devlog is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; devlog is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with devlog; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.



;;; Description:

;; This is a quick and dirty development log. A lot of the ideas come
;; out of _Practical Common Lisp_, although they've been ported to
;; emacs lisp. And made different. Yeah.

;; Let me know if you have any feature requests, or if you add any
;; yourself. I'll do my best to put them in.



;;; Usage:

;; tasks are added with the `devlog-task' function. this function will
;; update the development log with the task you give it.

;; To use this, pop the file somewhere in your path and add this to your
;; .emacs file:

;; (require 'devlog)



;;; Implementation Notes:

;; This is my first emacs lisp code. ever. Expect it to be non-lispy.



;;; Code:

(defgroup devlog nil
  "devlopment log functions")

(defvar devlog-version "0.1"
  "The current version number of devlog")

(defvar *devlog* nil "the development log")
(defvar devlog-file "~/.devlog")

;;; ===================================
;;; helper functions
;;; ===================================
(defun devlog-format-datetime (&optional time)
  (format-time-string "%Y-%m-%d %H:%M" time))

(defun devlog-format-time (&optional time)
  (format-time-string "%H:%M" time))

(defun devlog-make-task (description)
  (list :description description :start (current-time) :stop nil))

(defun devlog-add-task (task)
  (push task *devlog*))

(defun devlog-write-nil-file ()
  (with-current-buffer (find-file-noselect devlog-file)
    (insert "nil")
    (save-buffer)
    (kill-buffer nil)))

(defun devlog-ensure-file ()
  (if (file-exists-p devlog-file)
      nil
    (devlog-write-nil-file)))

(defun devlog-load ()
  (devlog-ensure-file)
  (with-current-buffer (find-file-noselect devlog-file)
    (setf *devlog*
          (car (read-from-string (buffer-string))))))

(defun devlog-save ()
  (devlog-ensure-file)
  (with-current-buffer (find-file-noselect devlog-file)
    (erase-buffer)
    (insert (prin1-to-string *devlog*))
    (save-buffer)
    (kill-buffer nil)))

(defun devlog-clear-all-tasks ()
  (setf *devlog* nil))

(defun devlog-stop-all-tasks ()
  (setf *devlog*
        (mapcar
         #'(lambda (task)
             (if (equal (getf task :stop task) nil)
                 (setf (getf task :stop) (current-time)))
             task)
         *devlog*)))

(defun devlog-elapsed-time (task)
  (time-subtract (getf task :start) (getf task :stop)))

(defun devlog-dump-tasks ()
  (dolist
      (task (sort *devlog* #'(lambda (x y) (time-less-p (getf x :start) (getf y :start)))))
    (if (equal (getf task :stop) nil)
        (insert
         (format "%s-%s (%s)\t%s\n"
                 (devlog-format-time (getf task :start))
                 (devlog-format-time (getf task :stop))
                 (devlog-format-time (devlog-elapsed-time task))
                 (getf task :description))))))

;;; ===================================
;;; interactive functions
;;; ===================================
(defun taskfill ()
  (interactive)
  (devlog-clear-all-tasks)
  (devlog-add-task (devlog-make-task "yummy"))
  (devlog-add-task (devlog-make-task "yummy"))
  (devlog-add-task (devlog-make-task "yummy"))
  (devlog-add-task (devlog-make-task "yummy"))
  (devlog-save))

(defun devlog-open ()k
  "open the development log database for viewing"
  (interactive)
  (find-file devlog-file))

(defun devlog-task ()
  "Add a task to the development log"
  (interactive)
  (devlog-load)
  (devlog-stop-all-tasks)
  (devlog-add-task (devlog-make-task
                    (read-string "Task: ")))
  (devlog-save))

(defun devlog-stop ()
  "Stop all active tasks in the development log"
  (interactive)
  (devlog-load)
  (devlog-stop-all-tasks)
  (devlog-save))

(defun devlog-clear ()
  "Clear all tasks from the development log"
  (interactive)
  (if (y-or-n-p "Are you sure you want to clear the development? ")
      (progn
        (devlog-load)
        (devlog-clear-all-tasks)
        (devlog-save))))

(defun devlog-summary ()
  "Summarize the development log"
  (interactive)
  (devlog-load)
  (set-buffer
   (get-buffer-create "development-log"))
  (switch-to-buffer "development-log")
  (erase-buffer)

  (insert "All Tasks\n=========\n")
  (devlog-dump-tasks)

  (insert "\n")
  (insert "Breakdown\n=========\n")
  ;; for each unique task
  ;; sum the time spent
  ;; print the line

  )





;; 2008-01-01
;; ----------
;; 15:20-16:30  (01:10) task one
;; 16:30-17:00  (00:30) task two
;; 17:00-18:00  (01:00) task one

;; 2008-01-02
;; ----------
;; 15:20-16:30  (01:10) task one
;; 16:30-17:00  (00:30) task two
;; 17:00-18:00  (01:00) task one


;; breakdown
;; ---------
;; task one     04:20 elapsed
;; task two     01:00 elapsed
