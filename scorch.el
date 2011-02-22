;;; scorch.el --- minor mode for editing Code Igniter projects
;; Copyright (c) 2008 Trevor Oke <trevor@trevoroke.com>
;; Authors: Trevor Oke <trevor@trevoroke.com>
;; Keywords: Code Igniter, CI, languages, framework

;; GPL thinger

(unless (<= 22 emacs-major-version)
  (error
   (format "scorch-mode requires version 22 of emacs. You are currently running version %s.%s"
           emacs-major-version
           emacs-minor-version)))

(eval-when-compile
  (require 'php-mode))

(define-minor-mode scorch-minor-mode
  "CodeIgniter"
  nil
  "CI")

(provide 'scorch)