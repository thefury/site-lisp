(slime-register-lisp-implementation
  "clisp-2.33.2"
  (lispbox-list-to-filename 
    (list
      (file-name-directory load-file-name) "bin" (format "clisp -ansi -K full -B %s" (lispbox-list-to-filename (list (file-name-directory load-file-name) "lib" "clisp"))))))
