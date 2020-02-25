;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Author: Nan Wu

;;; Code:
(setq gc-cons-threshold (* 800 1024))

;; Bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defconst straight-cache-autoloads t)
(defconst straight-use-package-by-default t)
(defconst straight-check-for-modifications 'live)
;; (require 'straight bootstrap-file t)

;; (defconst use-package-verbose t)
;; (straight-use-package 'use-package)
;; (straight-use-package 'bind-map)

(eval-when-compile
  (require 'recentf)
  (require 'use-package))


;; (defalias 'use-package-handler/:ensure #'use-package-handler/:straight)
;; (defalias 'use-package-normalize/:ensure #'use-package-normalize/:straight)

(require 'seq)
(require 'subr-x)

(defun nw-init/init-load-path (&optional interactive-p)
  "Add select subdirs of `user-emacs-directory' to the `load-path'.
If argument INTERACTIVE-P is set, log additional information."
  (interactive "p")
  (let* ((before load-path)
         (lisp-dir (expand-file-name "lisp" user-emacs-directory))
         (config-dir (expand-file-name "config" user-emacs-directory))
         (git-subtrees
          (seq-filter #'file-directory-p
                      (directory-files lisp-dir t "^[^.]")))
         (config-subtrees
          (seq-filter #'file-directory-p
                      (directory-files config-dir t "^[^.]"))))
    (dolist (path (append (list lisp-dir config-dir) config-subtrees git-subtrees))
      (add-to-list 'load-path path)
      (add-to-list 'Info-default-directory-list path)
      (add-to-list 'load-path (concat path "/emacs"))
      (add-to-list 'load-path (concat path "/elisp"))
      (add-to-list 'load-path (concat path "/lisp")))

    (setq load-path (seq-filter #'file-directory-p load-path))
    (setq Info-default-directory-list (seq-filter #'file-directory-p Info-default-directory-list))

    (when interactive-p
      (if-let (added (seq-difference load-path before))
          (message "Load path updated. Added: %S" added)
        (message "No change to load-path")))))

(nw-init/init-load-path)

;; major mode hydra
;(require 'major-mode-hydra)

;; Load features.
(straight-use-package nw-base)
;; (use-package nw-evil)
;; (use-package nw-ivy)
;; (use-package nw-projectile)
;; (use-package nw-eyebrowse)
;; (use-package nw-magit)
;; (use-package nw-yaml)
;; (use-package nw-markdown)
;; (use-package nw-lsp)
;; (use-package nw-company)
;; (use-package nw-flycheck)
;; (use-package nw-python)


;(use-package jp-smartparens)
;(use-package jp-ivy)
;(use-package jp-company)
;(use-package jp-flycheck)
;(use-package jp-projectile)
;
;(use-package jp-magit)
;(use-package jp-restclient)
;(use-package jp-yasnippet)
;(use-package jp-org)

;(use-package jp-docker)
;(use-package jp-yaml)
;(use-package jp-markdown)

;; Programming language support
;(use-package jp-go)
;(use-package jp-scala)
;(use-package jp-clojure)
;(use-package jp-php)
;(use-package jp-puppet)
;(use-package jp-protobuf)


(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7ef2884658a1fed818a11854c232511fa25721c60083a2695e6ea34ce14777ee" default)))
 '(package-selected-packages (quote (flycheck)))
 '(safe-local-variable-values
   (quote
    ((eval progn
	   (aggressive-indent-mode -1)
	   (define-clojure-indent
	     (expect 0)
	     (expect-search 0)
	     (expect-next 0)))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
