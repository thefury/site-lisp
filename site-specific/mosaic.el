;;; pomodoro.el --- Pomodoro Technique for emacs

;;; Commentary:

;; The technique is described in http://www.pomodorotechnique.com
;;
;; to start the pomodoro you issue the following command:
;;
;; M-x pomodoro
;;
;; in the modeline you will see the following indicator W1-25. This
;; means that you are working on set 1 and that you have 25 minutes
;; remaining. The counter will decrease each minutes. When it reaches
;; 0 you will get a message in a buffer that it's time to take a
;; break. The modeline will display B1-5, that is you have a break of
;; 5 minutes. When the count reaches 0 you will another message to get
;; back to work and the set number will increase. At the end of the
;; 4th set you will get a long break. The modeline will display LB
;; instead of B.
;;
;; When you don't need the pomodoro anymore you do:
;;
;; M-x pomodoro-stop
;;
;; I you got interrupted and you want to rewind the pomodoro on the
;; current set just do:
;;
;; M-x pomodoro-rewind
;;
;; Calling M-x pomodoro again will reset it to the first working set
;;
;; You can customize this mode with the following variables:
;;  - `pomodoro-work-time' number of minutes of working
;;  - `pomodoro-short-break' number of minutes of a short break
;;  - `pomodoro-long-break' number of minutes of a long break
;;  - `pomodoro-set-number' number of sets until a long break
;;  - `pomodoro-set-number' the minutes of a working set with

;;; THANKS:

;; Obviously Francesco Cirillo for creating the technique. I was
;; inspired by a pomodoro timer for Windows but I can't find out who
;; wrote it...
;; Richard Riley for pointing out I forgot provide
;; Manan for some patches

;;; BUGS:

;; Are you kidding me? This software is perfect ;)

;;; INSTALLATION:

;; To activate this package simply put this file in your load path.
;; For example if you put the file is in the directory ~/tmp you need
;; to do the following :
;;
;; (add-to-list 'load-path "~/tmp")
;; (require 'pomodoro)
;;

;;; Code:

(defun standup-filename ()
  (format "%s/%s.txt" (expand-file-name "~/Documents/standup-meetings") (format-time-string "%d-%m-%Y")))


(defun insert-checklist (arg1)
  (insert arg1)
  (newline))

(defun checklist ()
  (interactive)
  (get-buffer-create "*code-checklist*")
  (switch-to-buffer "*code-checklist*")
  (org-mode)
  (insert-checklist "* Code Checklist")
  (insert-checklist "  - [ ] Have you ensured that each controller action calls at most 1 extra model method?")
  (insert-checklist "    - create a custom find method instead.")
  (insert-checklist "  - [ ] Is there a maximum of 2 instance variables shared between Action and View?")
  (insert-checklist "  - [ ] Are all model and controller names immediately obvious? ")
  (insert-checklist "  - [ ] Are any custom finds called more than once? Convert to a named scope.")
  (insert-checklist "  - [ ] Is find or find_by called in a view? Remove to controller or model.")
  (insert-checklist "    - exceptions for select boxes")
  (insert-checklist "  - [ ] Has all costum functionality been checked for framework duplication?")
  (insert-checklist "  - [ ] Is this code duplicated between modules? Convert into a library/module.")
  (insert-checklist "  - [ ] Is this code useful in other situations? Convert to plugin/gem.")
  (insert-checklist "  - [ ] Have all guesses at future functionality been removed?")
  (insert-checklist "  - [ ] Has a regression test been added to ensure an issue does not recur?")
  (insert-checklist "  - [ ] Does the code pass all tests?")
  (insert-checklist "  - [ ] Have you run Rails Best Practices on the code?"))

(defun standup ()
  (interactive)
  (with-temp-buffer
    (end-of-buffer)
    (insert (concat "Standup meeting for: " (format-time-string "%d-%m-%Y")))
    (newline)
    (insert-file (expand-file-name "~/Documents/standup-meetings/template.txt"))
    (newline)
    (with-temp-message (concat "writing standup file to " (standup-filename))
      (when (file-writable-p (standup-filename))
        (write-region (point-min)
                      (point-max)
                      (standup-filename)))))
  (message (concat "standup file " (standup-filename) " created"))
  (find-file (standup-filename)))


(provide 'mosaic)
