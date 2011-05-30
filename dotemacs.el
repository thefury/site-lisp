(defun add-path (p)
  (add-to-list 'load-path (concat site-lisp p)))

(add-to-list 'load-path site-lisp)


(add-path "/basics")
(require 'common-system)
(require 'common-aliases)
(require 'common-functions)
(require 'appearance)

(add-path "/logging")
(require 'exercise-log)

(add-path "/site-specific")
(require 'mosaic)

(add-path "/modes")
(require 'fury-org-mode)
(require 'fury-rails-mode)


(add-path "/mmm-mode-0.4.8")
(add-path "/nxml-mode-20041004")
(add-path "/g-client")
(add-path "/remember")



;;; ===== pomodomo mode
(add-to-list 'load-path "~/tmp")
(require 'pomodoro)

;;; ===== ido mode
(require 'ido)
(ido-mode t)

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
 '(safe-local-variable-values (quote ((espresso-indent-level . 4)))))

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






