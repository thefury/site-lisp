(add-path "/emacs-rails")
(add-path "/feature-mode")

(require 'ruby-mode nil 't)
(require 'rails nil 't)
(require 'yaml-mode)
(require 'haml-mode nil 't)
(require 'sass-mode nil 't)
(require 'feature-mode)
(require 'css-mode)


(setq auto-mode-list
      (append '(("\\.rb$" . ruby-mode)
                ("\\.yml$" . yaml-mode)
                ("\\.haml$" . haml-mode)
                ("\\.sass$" . sass-mode)
                ("\\.css$" . css-mode)
                ("\\.feature$" . feature-mode)
                ("\\.html$" . html-mode))
              auto-mode-alist))

(provide 'fury-rails-mode)