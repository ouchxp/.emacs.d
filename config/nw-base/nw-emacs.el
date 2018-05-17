(defconst nw-emacs-cache-directory
  (concat user-emacs-directory ".cache"))

;; better scrolling
(setq scroll-preserve-screen-position t)
(setq scroll-margin 0)
(setq scroll-conservatively 101)

(delete-selection-mode 1)

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

(provide 'nw-emacs)