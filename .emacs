(defun system-is-windows ()
  "Are we running a windows box?"
  (string-match "windows" system-configuration))

(defun system-is-mac ()
  "Are we running a mac?"
  (string-match "darwin" system-configuration))

;; ============================
;; Where to find things
;; ============================

(defvar site-lisp "~/.site-lisp")
(defvar dotemacs "~/.emacs")

;; ============================
;; elisp path
;; ============================

(defun add-path (p)
  (add-to-list 'load-path (concat site-lisp p)))

(add-to-list 'load-path site-lisp)
(add-path "/emacs-rails")
(add-path "/mmm-mode-0.4.8")
(add-path "/nxml-mode-20041004")
(add-path "/g-client")
(add-path "/remember")
(add-path "/feature-mode")


(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)
(global-set-key (kbd "<S-f1>")  'twitter-get-friends-timeline)

;; ============================
;; Aliases
;; ============================

(defalias 'qrr 'query-replace-regexp)
(defalias 'yes-or-no-p 'y-or-n-p)

;; ============================
;; Copy And Paste from OSX
;; ============================
(when (system-is-mac)
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))


;; ============================
;; Helper Functions
;; ============================
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

(defun work ()
  (interactive)
  (find-file "~/projects/work/mpower"))
(global-set-key (kbd "<f7>")  'work)

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

(defun insert-cover-letter (manager-name)
  (interactive "sHiring Manager: ")
  (end-of-buffer)
  (insert-file "~/Documents/Resumes/cover-letter-template")
  (end-of-buffer)
  (insert "manager-name:" manager-name "\n")
  (insert "manager-name-length:" (length manager-name) "\n")
  (if (= (length manager-name) 0)
      (insert "hiring manager: " manager-name)
    (insert "*** hiring manager: no manager")))


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

(defun date-string ()
  (interactive)
  (format-time-string "[%Y-%m-%d]"))

(defun insert-date ()
  (interactive)
  (insert (date-string)))

(defalias 'qrr 'query-replace-regexp)



;; basic colors
(set-cursor-color "red")
(set-foreground-color "ForestGreen")
(set-background-color "black")

;; Turn on pretty font colors
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)


;; Scroll faster, colors won't appear correctly right away
(setq lazy-lock-defer-on-scrolling t)

(setq search-highlight t)              ; incremental search highlights
(setq query-replace-highlight t)        ; highlight during query

;; set highlight color
(set-face-background 'region "MidnightBlue")

;; Isearch highlight colors
(copy-face 'default  'isearch)
(set-face-background 'isearch "DarkGreen")
(set-face-foreground 'isearch "white")

;; Isearch lazy highlight colors
(if (> emacs-major-version 20)
    (progn (set-face-background 'isearch-lazy-highlight-face "DimGray")
           (set-face-foreground 'isearch-lazy-highlight-face "white")))

;; Key Bindings
(global-set-key (kbd "<f6>")  'replace-string)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\M-s" 'isearch-forward-regexp)
(global-set-key "\M-r" 'isearch-backward-regexp)
(global-set-key "\C-z" 'undo)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c g") 'open-gtd)


;; disable startup message
(setq inhibit-startup-message t)

;; setup font
(when (system-is-windows)
  (set-default-font
   "-Misc-Fixed-Medium-R-Normal--15-140-75-75-C-90-ISO8859-1"))

;; display the current time
(display-time)

;; Show column number at bottom of screen
(column-number-mode 1)


;; highlight matches from searches
(setq isearch-highlight t)
(setq search-highlight t)
(setq-default transient-mark-mode t)

(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))



;; Turn on selection
(setq transient-mark-mode 't highlight-nonselected-windows 't)

;; Show line and column number
(line-number-mode 1)
(column-number-mode 1)

;; Don't beep at me
(setq visible-bell t)

;;; ===== ido mode
(require 'ido)
(ido-mode t)

;;; ===== Set up yasnippets
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.site-lisp/snippets/")

;; ===== Ruby and Rails
(setq auto-mode-alist (cons '("\\.rb\\'" . ruby-mode) auto-mode-alist))

(require 'ruby-mode nil 't)
(require 'rails nil 't)

;; ===== YAML Mode
(require 'yaml-mode)
(setq auto-mode-alist (cons '("\\.yml\\'" . yaml-mode) auto-mode-alist))

;; ===== HAML Mode
(require 'haml-mode nil 't)
(setq auto-mode-alist (cons '("\\.haml\\'" . haml-mode) auto-mode-alist))

;; ===== SASS Mode
(require 'sass-mode nil 't)
(setq auto-mode-alist (cons '("\\.sass\\'" . sass-mode) auto-mode-alist))

;; ===== PHP Mode
(require 'php-mode nil 't)
(setq auto-mode-alist (cons '("\\.php\\'" . php-mode) auto-mode-alist))
(add-hook 'php-mode-user-hook 'turn-on-font-lock)

;; ===== Feature Mode
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; ============================
;; MMM Mode
;; ============================
(require 'mmm-mode)
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)
(mmm-add-classes
 '((erb-code
    :submode ruby-mode
    :match-face (("<%#" . mmm-comment-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :front "<%[#=]?"
    :back "-?%>"
    :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
             (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
             (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @))
    )))
(add-hook 'html-mode-hook
          (lambda ()
            (setq mmm-classes '(erb-code))
            (mmm-mode-on)))
(add-to-list 'auto-mode-alist '("\.rhtml$" . html-mode))

;; ===== ORG Mode
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
;; ===== CSS Mode
(require 'css-mode)
(autoload 'css-mode "css-mode")
(setq auto-mode-alist
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; ===== Subversion
(require 'psvn)


;; ===== Web Searches
(require 'webjump)
(global-set-key [f2] 'webjump)
(setq webjump-sites
      (append '(
                ("Reddit Search" .
                 [simple-query "www.reddit.com" "http://www.reddit.com/search?q=" ""])
                ("Google Image Search" .
                 [simple-query "images.google.com" "images.google.com/images?hl=en&q=" ""])
                ("Flickr Search" .
                 [simple-query "www.flickr.com" "flickr.com/search/?q=" ""])
                )
              webjump-sample-sites))

;; ===== GNUS Gmail
(setq startttls-use-gnu-tls t)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)
(require 'smtpmail)

;; weblogger
(require 'weblogger)

;; ===== Custom Set Variables
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(canlock-password "f436c714e2f497b0fc151be30db462100483ffc6")
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(org-agenda-files (quote ("~/GTD/gtd.org")))
 '(rails-ws:default-server-type "lighttpd")
 '(weblogger-config-alist (quote (("mr-furious" ("user" . "admin") ("server-url" . "http://mr-furious.com/xmlrpc.php") ("weblog" . "1")) ("trevoroke" ("user" . "trevor") ("server-url" . "http://trevoroke.com/xmlrpc.php") ("weblog" . "1"))))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; ===== More File Associations
(setq auto-mode-list
      (append '(("\\.C$" . c++-mode)
                ("\\.cc$" . c++-mode)

                ("\\.xml$" . nxml-mode)
                ("\\.xsl$" . nxml-mode)
                ("\\.rng$" . nxml-mode)

                ("\\.xhtml$" . nxml-mode)
                ("\\.html$" . html-mode))

              auto-mode-alist))

;; ===== Keyboard macros

;; Take all of the bug reports and make a condensed table
(fset 'wp-bugreport-to-table
      [?\C-s ?d ?a ?t ?e ?/ ?t ?i ?m ?e C-right C-left ?\C-  home ?\C-x ?\C-k ?| ?  end ?  ?| ?\C-s ?e ?x ?c ?e ?p ?t ?i ?o ?n ?  ?c ?l ?a ?s ?s C-right C-left ?\C-  up up up up up up up up up up up up up up up up up up up up up end ?\C-x ?\C-k ?  end ?  ?| right backspace ?  end ?  ?| home down ?\C-s ?d ?a ?t ?e ?/ ?t ?i ?m ?e ?\C-z ?  ?| down ?\C-  ?\C-s ?d ?a ?t ?e ?/ ?t ?i ?m ?e C-left C-left ?\C-x ?\C-k home])



;; === more functions
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

(defun log-work (project description)
  (interactive "sProject: \nsDescription: ")
  (find-file "~/Documents/Work/work-log.org")
  (end-of-buffer)
  (insert
   (format "* %s %s: %s\n"
           (date-time-string) project description))
  (save-buffer))


(defun insert-new-block ()
  (interactive)
  (insert "#content_block\n")
  (insert "  #main_content\n")
  (insert "\n")
  (insert "- content_for :titlebar do\n")
  (insert "  %h3.moduletitle \n")
  (insert "\n")
  (insert "- content_for :sidebar do\n")
  (insert "  #content_nav\n")
  (insert "    TODO placeholder\n"))


(defun guitar-insert-tab-line ()
  (interactive)
  (insert "\n")
  (insert "e |----------------|----------------|----------------|----------------|\n")
  (insert "B |----------------|----------------|----------------|----------------|\n")
  (insert "G |----------------|----------------|----------------|----------------|\n")
  (insert "D |----------------|----------------|----------------|----------------|\n")
  (insert "A |----------------|----------------|----------------|----------------|\n")
  (insert "E |----------------|----------------|----------------|----------------|\n\n"))

(put 'narrow-to-region 'disabled nil)

(defun add-require (module)
  (interactive "sRequire statement: require ")
  (save-excursion
    (goto-char (point-max))
    (condition-case nil
        (re-search-backward "^\\(require .+;\\|#!/\\)")
      (error (goto-char 0)))
    (end-of-line)
    (insert (concat "\nrequire \"" module "\""))))


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


;;; ========== Aliases
(defalias 'rs 'replace-string)
(defalias 'fd 'find-dired)
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
