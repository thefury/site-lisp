
;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(defun log-workout (exercise sets reps weight)
  (interactive "sExercise: \nNSets: \nNReps: \nNWeight(kg):")
  (find-file "~/Documents/Exercise/exercise-log.org")
  (end-of-buffer)
  (insert (format "| %s | %s | %d | %d | %d | %d | %d |\n"
                  (date-string)
                  exercise sets reps weight
                  (+ weight 20)
                  (* sets reps (+ weight 20))))
  (save-buffer))

(defun log-weight (weight)
  (interactive "NWeight: ")
  (find-file "~/Documents/Exercise/weight-log.org")
  (end-of-buffer)
  (insert
   (format "| %s | %d | %.2f |\n"
           (date-string)
           weight
           (* weight 0.453)))
  (save-buffer))

(defun 1rm (weight reps)
  (/ weight (- 1.0278 (* 0.0278 reps))))

(defun 1rep-max (weight reps)
  (interactive "nWeight: \nnReps:")
  (print (format "Your 1RM - %.2f"
                 (1rm weight reps))))

(defun next-lift (weight reps)
  (interactive "nWeight: \nnReps:")
  (print (format "you should try for %.2f for 3-5 reps"
                 (* 0.8 (1rm weight reps)))))

(provide 'exercise-log)