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
(global-set-key (kbd "<f5>") (quote comment-or-uncomment-region)) ;; Not (quote comment-region)
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "ForestGreen" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "apple" :family "Monaco")))))

(provide 'appearance)