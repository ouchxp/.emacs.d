;;; nw-lsp-python.el --- Configuration for Python LSP.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'dash)
(autoload 'projectile-project-root "projectile")
(autoload 'lsp-define-stdio-client "lsp-mode")

(defvar lsp-python nil "Name of Python LSP Client.")

(defun nw-lsp-python--find-python-root ()
  "Return the current Python project root, if any.
This is marked with setup.py or setup.cfg."
  (or (locate-dominating-file default-directory "setup.py")
      (locate-dominating-file default-directory "setup.cfg")))

(defun nw-lsp-python--find-git-root ()
  "Return the current git repository root, if any."
  (locate-dominating-file default-directory ".git"))

(defun nw-lsp-python--find-projectile-root ()
  "Return the current project root according to projectile."
  ;; `ignore-errors' both to avoid an unbound function error as well
  ;; as ignore projectile saying there is no project root here.
  (ignore-errors
    (projectile-project-root)))

(defun nw-lsp-python--find-root ()
  (cond ((nw-lsp-python--find-python-root)
         (nw-lsp-python--find-projectile-root)
         (nw-lsp-python--find-git-root))))

(defun nw-lsp-python--get-lsp-root ()
  (if (nw-lsp-python--find-root) (nw-lsp-python--find-root) default-directory))

(defun nw-lsp-python--setup ()
  "Setup Python LSP client."
  (lsp-define-stdio-client lsp-python "python" #'nw-lsp-python--get-lsp-root '("pyls")))

(provide 'nw-lsp-python)

;;; nw-lsp-python.el ends here
