(require 'org)

(setq org-agenda-custom-commands
      '(("P" "Project List"
         ((tags "PROJECT") ))
        ("O" "Office"
         ((agenda)
          (tags-todo "OFFICE")))
        ("W" "Weekly Plan"
         ((agenda)
          (todo "TODO")
          (tags "PROJECT")))
        ("H" "Home NA Lists"
         ((agenda)
          (tags-todo "HOME")
          (tags-todo "COMPUTER")))))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))        ;; (4)
(setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE")) ;; (6)
(setq org-agenda-include-diary t)                             ;; (7)
(setq org-agenda-include-all-todo nil)
(define-key org-mode-map [tab] 'org-table-next-field)

;; ===== remember mode
(setq org-directory "~/GTD/")
(setq org-default-notes-file "~/.notes")
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map "\C-cr" 'org-remember)

                                        ; for templates, etc.
                                        ; http://members.optusnet.com.au/~charles57/GTD/remember.html
(setq org-remember-templates
      '(("Todo" ?t "* TODO %^{Brief Description} %^g" "~/GTD/gtd.org" "Tasks")
        ("Journal"   ?j "** %^{Head Line} %U %^g\n%i%?"  "~/GTD/journal.org")
        ("Book" ?b "** %^{Book Title} %t :BOOK: \n%[~/.book_template.txt]\n"
         "~/GTD/journal.org")
        ("Film" ?f "** %^{Film Title} %t :FILM: \n%[~/.film_template.txt]\n"
         "~/GTD/journal.org")
        )
      )

(provide 'fury-org-mode)