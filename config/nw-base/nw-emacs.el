(defconst nw-emacs-cache-directory
  (concat user-emacs-directory ".cache"))

;; better scrolling
(setq scroll-preserve-screen-position t)
(setq scroll-margin 0)
(setq scroll-conservatively 101)

(provide 'nw-emacs)