
(eval-when-compile
  (require 'use-package))

(use-package company
  :commands (global-company-mode)

  :bind
  (("S-<return>" . company-complete))

  :preface
  (defun nw-company--set-company-vars ()
    (setq company-minimum-prefix-length 3)
    (setq company-tooltip-align-annotations t))

  :commands (company-select-next
             company-select-previous
             company-show-doc-buffer)

  :init
  (add-hook 'after-init-hook #'global-company-mode)

  :config
  (progn
    (setq company-idle-delay 0.3)
    (setq company-require-match nil)

    (dolist (map (list company-active-map company-search-map company-filter-map))
      (define-key map (kbd "C-j") #'company-select-next)
      (define-key map (kbd "C-k") #'company-select-previous)
      (define-key map (kbd "C-h") #'company-show-doc-buffer)
      (define-key map (kbd "C-w") nil))

    (add-hook 'company-mode-hook #'nw-company--set-company-vars)))

(use-package company-dabbrev
  :after company
  :config
  (progn
    (setq company-dabbrev-ignore-case nil)
    (setq company-dabbrev-downcase nil)))

(use-package company-lsp
  :after company
  :defines company-lsp
  :preface
  (progn
    (defun nw-company--lsp-mode-p ()
      (and (bound-and-true-p lsp-mode)
           (bound-and-true-p company-mode)))
    (defun nw-company--setup-lsp-backend ()
      (when (nw-company--lsp-mode-p)
        (set (make-local-variable 'company-backends) '(company-lsp)))))
  :config
  (add-hook 'company-mode-hook #'nw-company--setup-lsp-backend))

;;; TODO: Set this up properly
;; (use-package company-box
;;   :after company
;;   :commands (company-box-mode
;;              company-box--next-line
;;              company-box--prev-line)
;;   :defines company-box-mode-map
;;   :config
;;   (progn
;;     (define-key company-box-mode-map (kbd "C-j") #'company-box--next-line)
;;     (define-key company-box-mode-map (kbd "C-k") #'company-box--prev-line)
;;     (company-box-mode +1)))

(provide 'nw-company)
